# =============================================================================
# validate_gar.R: is player GAR measuring something real?
#
# Run AFTER player_gar.R. Each check asks a question GAR was NOT fitted to, and
# compares GAR with a simple baseline (raw goal difference, xG + xA per 90, ...).
# A useful model should beat the baselines; if it doesn't, it's adding noise.
#
#   1. Out-of-sample games   Does team GAR from conference games predict
#                            NON-conference results (games not in the data)?
#   2. Outside judgment      Does team GAR line up with Massey team ratings?
#   3. Coaches' choices      Within a team and position, does the higher-GAR
#                            player get more minutes?
#   4. Reliability           How much of the spread in GAR is signal vs noise?
#                            How many players can we actually tell apart?
#   5. Team-strength leak    Is GAR mostly crediting players for their team?
#   6. Sensitivity           Do rankings survive reasonable changes to the
#                            model's settings? (re-runs player_gar.R; ~3 min)
#   7. Year to year          Does GAR repeat next season? (needs 2+ seasons)
#   8. Awards (optional)     Do All-Conference / All-American players rate high?
#                            Fill in awards.csv (a template is written).
#   9. True talent           Simulated data only: the answer key.
#
# Outputs (validation_output/): validation_summary.csv, one chart per check,
# and a printed report.
#
# Run:  Rscript validate_gar.R        (from the folder with player_gar.R)
# Packages: readr, dplyr, tidyr, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---- Settings ---------------------------------------------------------------
DATA_SOURCE     <- "real"          # "real" or "simulated" (match player_gar.R)
RUN_SENSITIVITY <- TRUE            # re-run player_gar.R with other settings (slow)
N_BOOT          <- 1000            # bootstrap samples for confidence intervals
OUT_DIR         <- "validation_output"
AWARDS_FILE     <- "awards.csv"    # optional: player_name, team, award
FOCUS_TEAM      <- "Virginia Tech"

if (DATA_SOURCE == "real") {
  GAR_DIR <- "GAR_output_real"; PLAYERS_FILE <- "real_player_metrics.csv"
  STANDINGS_FILE <- "real_standings.csv"; RPI_FILE <- "rpi_standings_all_fixed.csv"
  QUAL_MIN <- 360; PRIOR_MIN <- 270
} else {
  GAR_DIR <- "GAR_output"; PLAYERS_FILE <- "simulated_player_metrics.csv"
  STANDINGS_FILE <- "simulated_standings.csv"; RPI_FILE <- NA
  QUAL_MIN <- 600; PRIOR_MIN <- 270
}

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE)
MAROON <- "#861F41"; ORANGE <- "#E5751F"; GREY <- "#B9B3AE"; SLATE <- "#44546A"
theme_v <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        plot.title.position = "plot", legend.position = "top", legend.justification = "left")

summary_rows <- list()
add_result <- function(check, measure, gar, baseline = NA, baseline_name = NA, verdict, meaning) {
  summary_rows[[length(summary_rows) + 1]] <<- tibble(
    check, measure, gar = round(gar, 3), baseline = round(baseline, 3), baseline_name, verdict, meaning)
}
boot_ci <- function(x, y, f = function(a, b) cor(a, b), n = N_BOOT) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  b <- replicate(n, { i <- sample(seq_along(x), replace = TRUE); f(x[i], y[i]) })
  quantile(b, c(0.05, 0.95), na.rm = TRUE)
}
# Same small-sample shrinkage the model uses, so baselines get a fair comparison
shrink <- function(x, minutes, group) {
  ave_w <- ave(seq_along(x), group, FUN = function(i) {
    q <- i[minutes[i] >= QUAL_MIN & !is.na(x[i])]
    rep(if (length(q)) weighted.mean(x[q], minutes[q]) else mean(x[i], na.rm = TRUE), length(i))
  })
  x <- coalesce(x, ave_w)
  (x * minutes + ave_w * PRIOR_MIN) / (minutes + PRIOR_MIN)
}

# ---- Load -------------------------------------------------------------------
need <- c(file.path(GAR_DIR, c("player_gar.csv", "team_gar.csv")), PLAYERS_FILE, STANDINGS_FILE)
miss <- need[!file.exists(need)]
if (length(miss)) stop("Missing: ", paste(miss, collapse = ", "), ". Run player_gar.R first.")

gar   <- read_csv(file.path(GAR_DIR, "player_gar.csv"), show_col_types = FALSE)
tgar  <- read_csv(file.path(GAR_DIR, "team_gar.csv"), show_col_types = FALSE)
mets  <- read_csv(PLAYERS_FILE, show_col_types = FALSE)
stand <- read_csv(STANDINGS_FILE, show_col_types = FALSE)
LATEST <- max(gar$season_year)
z0 <- function(x) coalesce(as.numeric(x), 0)
col_or_na <- function(df, nm) if (nm %in% names(df)) as.numeric(df[[nm]]) else NA_real_

# Simple per-90 baselines that anyone could compute without a model
base <- mets %>%
  transmute(season_year, player_id,
            xgxa_90  = z0(`xG / 90`) + z0(`xA / 90`),
            defact_90 = col_or_na(mets, "Successful defensive actions / 90"),
            prev_90  = col_or_na(mets, "Prevented goals / 90"),
            true_talent = col_or_na(mets, "true_talent"),
            team_strength_z = col_or_na(mets, "team_strength_z"))
pl <- gar %>%
  left_join(base, by = c("season_year", "player_id")) %>%
  group_by(season_year) %>%
  mutate(grp = paste(season_year, position),
         # naive baseline by position: xG + xA for attackers, defensive actions for
         # defenders, goals prevented for keepers (shrunk the same way as GAR)
         naive_raw = case_when(position == "GK" & !all(is.na(prev_90)) ~ prev_90,
                               position %in% c("CB", "FB", "DM") ~ defact_90,
                               TRUE ~ xgxa_90),
         naive = shrink(naive_raw, minutes_played, grp)) %>%
  ungroup()
cur <- filter(pl, season_year == LATEST)
qual <- filter(cur, minutes_played >= QUAL_MIN)
cat(sprintf("Validating %s/player_gar.csv: season %d, %d players (%d with %d+ minutes)\n\n",
            GAR_DIR, LATEST, nrow(cur), nrow(qual), QUAL_MIN))

# Team table
teams <- tgar %>% filter(season_year == LATEST) %>%
  mutate(gar_net_pg = (team_off_gaa + team_def_gaa) / games)
if (DATA_SOURCE == "real") {
  xg_team <- mets %>%
    mutate(xga = if_else(position == "GK", z0(col_or_na(mets, "xG against, total")), 0)) %>%
    group_by(team) %>%
    summarise(xg_for = sum(z0(`xG, Total`)), xg_against = sum(xga), .groups = "drop")
  teams <- teams %>%
    left_join(stand %>% select(team, team_rating, conference_rating), by = "team") %>%
    left_join(xg_team, by = "team") %>%
    mutate(xgd_pg = (xg_for - xg_against) / games)
}

# =============================================================================
# 1. Out-of-sample games: non-conference results
# =============================================================================
cat("1. OUT-OF-SAMPLE GAMES: does conference-game GAR predict non-conference results?\n")
if (!is.na(RPI_FILE) && file.exists(RPI_FILE)) {
  rpi <- read_csv(RPI_FILE, show_col_types = FALSE) %>% filter(season_year == LATEST) %>%
    transmute(team, nc_w = overall_w - conf_w, nc_l = overall_l - conf_l, nc_t = overall_t - conf_t,
              nc_g = nc_w + nc_l + nc_t, nc_win = (nc_w + 0.5 * nc_t) / nc_g)
  t1 <- teams %>% inner_join(rpi, by = "team") %>% filter(nc_g >= 4)
  preds <- c("Team GAR per game" = "gar_net_pg", "Conference goal difference per game" = "gd_pg",
             "Conference xG difference per game" = "xgd_pg", "Conference win %" = "win_pct")
  preds <- preds[preds %in% names(t1)]
  r1 <- tibble(predictor = names(preds), col = unname(preds)) %>%
    rowwise() %>%
    mutate(r = cor(t1[[col]], t1$nc_win, use = "complete.obs"),
           lo = boot_ci(t1[[col]], t1$nc_win)[1], hi = boot_ci(t1[[col]], t1$nc_win)[2]) %>%
    ungroup()
  print(r1 %>% select(predictor, r, lo, hi) %>% mutate(across(r:hi, ~ round(.x, 2))), n = Inf)
  m0 <- lm(nc_win ~ gd_pg, data = t1); m1 <- lm(nc_win ~ gd_pg + gar_net_pg, data = t1)
  inc <- summary(m1)$r.squared - summary(m0)$r.squared
  p_inc <- anova(m0, m1)$`Pr(>F)`[2]
  cat(sprintf("   Teams: %d, about %.0f non-conference games each.\n", nrow(t1), mean(t1$nc_g)))
  cat(sprintf("   GAR on top of goal difference: +%.3f R-squared (p = %.3f)\n\n", inc, p_inc))
  best_base <- r1 %>% filter(col != "gar_net_pg") %>% slice_max(r, n = 1)
  r_gar <- r1$r[r1$col == "gar_net_pg"]
  add_result("1. Non-conference games", "r with non-conference win %", r_gar, best_base$r, best_base$predictor,
             if (r_gar >= best_base$r) "GAR beats raw stats" else if (r_gar >= best_base$r - 0.05) "about equal" else "raw stats do better",
             "Out-of-sample games: the most honest team-level test")
  add_result("1. Non-conference games", "extra R-squared over goal difference", inc, NA, NA,
             if (p_inc < 0.05) "adds information" else "no clear extra information",
             sprintf("p = %.3f", p_inc))

  g1 <- ggplot(t1, aes(gar_net_pg, nc_win)) +
    geom_hline(yintercept = 0.5, colour = "grey85") +
    geom_point(colour = GREY, size = 2) +
    geom_point(data = filter(t1, team == FOCUS_TEAM), colour = MAROON, size = 3.5) +
    geom_text(data = filter(t1, team == FOCUS_TEAM), aes(label = team), colour = MAROON, vjust = -1, size = 3.5) +
    geom_smooth(method = "lm", se = TRUE, colour = MAROON, fill = "#EAD3DA", formula = y ~ x) +
    labs(title = "Conference-game GAR vs non-conference results",
         subtitle = sprintf("r = %.2f (90%% CI %.2f to %.2f). Non-conference games are not in the Wyscout data.",
                            r_gar, r1$lo[r1$col == "gar_net_pg"], r1$hi[r1$col == "gar_net_pg"]),
         x = "Team GAR per game (attack + defense, conference games)", y = "Non-conference win % (ties = half)") +
    theme_v
  g1b <- ggplot(r1, aes(r, reorder(predictor, r), fill = col == "gar_net_pg")) +
    geom_col(width = 0.6) + geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2) +
    scale_fill_manual(values = c(`TRUE` = MAROON, `FALSE` = GREY), guide = "none") +
    labs(title = "Which conference-game measure best predicts non-conference results?",
         subtitle = "Correlation with non-conference win %, with 90% bootstrap intervals",
         x = "Correlation (r)", y = NULL) + theme_v
  ggsave(file.path(OUT_DIR, "01a_nonconf_scatter.png"), g1, width = 8, height = 5.5, dpi = 150, bg = "white")
  ggsave(file.path(OUT_DIR, "01b_nonconf_vs_baselines.png"), g1b, width = 8, height = 3.6, dpi = 150, bg = "white")
} else cat("   Skipped: needs the RPI file's overall and conference records (real data).\n\n")

# =============================================================================
# 2. Outside judgment: Massey team ratings
# =============================================================================
cat("2. OUTSIDE JUDGMENT: does team GAR line up with Massey ratings?\n")
if ("team_rating" %in% names(teams) && any(!is.na(teams$team_rating))) {
  r_m <- cor(teams$gar_net_pg, teams$team_rating, use = "complete.obs")
  r_m_gd <- cor(teams$gd_pg, teams$team_rating, use = "complete.obs")
  ci <- boot_ci(teams$gar_net_pg, teams$team_rating)
  cat(sprintf("   r(team GAR, Massey) = %.2f (90%% CI %.2f to %.2f); r(goal difference, Massey) = %.2f\n\n",
              r_m, ci[1], ci[2], r_m_gd))
  add_result("2. Massey ratings", "r with Massey team rating", r_m, r_m_gd, "Conference goal difference per game",
             if (r_m >= r_m_gd) "GAR beats raw stats" else if (r_m >= r_m_gd - 0.05) "about equal" else "raw stats do better",
             "Massey uses the whole season (all games, opponents); partly shares data with GAR")
} else cat("   Skipped: no Massey ratings in the standings file.\n\n")

# =============================================================================
# 3. Coaches' choices: playing time within team and position
# =============================================================================
cat("3. COACHES' CHOICES: within a team and position, does the higher-rated player play more?\n")
pair_conc <- function(d, metric) {
  # share of same-team, same-position pairs where the higher-rated player has more minutes
  d %>% filter(minutes_played >= 90) %>%
    group_by(team, position) %>% filter(n() >= 2) %>%
    group_modify(function(g, k) {
      cmb <- t(combn(nrow(g), 2))
      dm <- sign(g$minutes_played[cmb[, 1]] - g$minutes_played[cmb[, 2]])
      dv <- sign(g[[metric]][cmb[, 1]] - g[[metric]][cmb[, 2]])
      tibble(conc = sum(dm * dv > 0), disc = sum(dm * dv < 0))
    }) %>% ungroup()
}
conc <- bind_rows(
  pair_conc(cur, "gar_per_90") %>% mutate(metric = "GAR per 90"),
  pair_conc(cur, "naive") %>% mutate(metric = "Simple stat")) %>%
  group_by(metric, position) %>%
  summarise(pairs = sum(conc + disc), concordance = sum(conc) / sum(conc + disc), .groups = "drop")
conc_all <- conc %>% group_by(metric) %>% summarise(concordance = weighted.mean(concordance, pairs), pairs = sum(pairs))
print(conc %>% select(-pairs) %>% pivot_wider(names_from = metric, values_from = concordance) %>%
        mutate(across(-position, ~ round(.x, 2))), n = Inf)
cg <- conc_all$concordance[conc_all$metric == "GAR per 90"]; cb <- conc_all$concordance[conc_all$metric == "Simple stat"]
cat(sprintf("   Overall: GAR %.2f vs simple stat %.2f (0.50 = no better than a coin flip)\n", cg, cb))
cat("   Simple stat = xG + xA per 90 (AM, CM, W, CF), successful defensive actions per 90 (CB, FB, DM),\n",
    "  goals prevented per 90 (GK). Note: low-minute players are pulled toward average, which helps both.\n\n")
add_result("3. Coaches' choices", "share of teammate pairs where higher rating = more minutes", cg, cb,
           "Simple stat by position",
           if (cg >= cb + 0.02) "GAR beats raw stats" else if (cg >= cb - 0.02) "about equal" else "raw stats do better",
           "Coaches see every practice; 0.50 = no agreement")
g3 <- ggplot(conc, aes(concordance, factor(position, levels = rev(c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF"))),
                       colour = metric)) +
  geom_vline(xintercept = 0.5, linetype = 2, colour = "grey60") +
  geom_line(aes(group = position), colour = "grey80", linewidth = 2) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("GAR per 90" = MAROON, "Simple stat" = SLATE), name = NULL) +
  labs(title = "Do coaches give more minutes to higher-rated players?",
       subtitle = "Share of same-team, same-position pairs where the higher-rated player played more (0.5 = coin flip)",
       x = "Agreement with coaches' playing-time choices", y = NULL) + theme_v
ggsave(file.path(OUT_DIR, "03_coaches_choices.png"), g3, width = 8, height = 4.8, dpi = 150, bg = "white")

# =============================================================================
# 4. Reliability: signal vs noise, and how many players we can tell apart
# =============================================================================
cat("4. RELIABILITY: how much of the real differences between players does GAR capture?\n")
# GAR is already shrunk toward the average for small samples, so each rating is a best guess
# with its own uncertainty (se_per_90). The spread of true values = spread of the best guesses
# + their average uncertainty, so the share GAR captures is
#   reliability = var(GAR) / (var(GAR) + mean(se^2))
# and sqrt(reliability) estimates the correlation between GAR and the player's true level.
rel <- qual %>% group_by(position) %>%
  summarise(n = n(), sd_gar = sd(gar_per_90), avg_uncertainty = sqrt(mean(se_per_90^2)),
            reliability = var(gar_per_90) / (var(gar_per_90) + mean(se_per_90^2)),
            r_with_truth = sqrt(reliability),
            .groups = "drop")
# Of the top 25 at each position, how often can we tell #k from #k+5?
sep <- qual %>% group_by(position) %>% arrange(desc(gar_per_90), .by_group = TRUE) %>% slice_head(n = 30) %>%
  mutate(p_vs_5_lower = pnorm((gar_per_90 - lead(gar_per_90, 5)) / sqrt(se_per_90^2 + lead(se_per_90, 5)^2))) %>%
  summarise(sure_vs_5_lower = mean(p_vs_5_lower >= 0.8, na.rm = TRUE), .groups = "drop")
rel <- rel %>% left_join(sep, by = "position")
print(rel %>% mutate(across(where(is.double), ~ round(.x, 2))), n = Inf)
rel_all <- weighted.mean(rel$reliability, rel$n)
cat(sprintf("   Reliability %.2f overall: GAR's correlation with players' true per-90 level is about %.2f\n",
            rel_all, sqrt(rel_all)))
cat("   'sure_vs_5_lower' = share of top-25 players we're 80% sure rate above the player 5 places below.\n\n")
add_result("4. Reliability", "share of real player differences GAR captures (0-1)", rel_all, NA, NA,
           if (rel_all >= 0.7) "good" else if (rel_all >= 0.5) "moderate: compare tiers, not exact ranks" else "low: rank in tiers, not exact order",
           sprintf("players with %d+ minutes; r with true level about %.2f. Counts sampling noise only, not model error, so this is a best case", QUAL_MIN, sqrt(rel_all)))
g4 <- ggplot(rel, aes(reliability, reorder(position, reliability))) +
  geom_col(fill = MAROON, width = 0.6) +
  geom_vline(xintercept = c(0.5, 0.7), linetype = 2, colour = "grey60") +
  geom_text(aes(label = sprintf("%.2f", reliability)), hjust = -0.2, size = 3.5) +
  scale_x_continuous(limits = c(0, 1.05)) +
  labs(title = "How much of the real differences between players does GAR capture?",
       subtitle = sprintf("Reliability by position, players with %d+ minutes (0.7+ good; below 0.5 = compare tiers, not exact ranks)", QUAL_MIN),
       x = "Reliability", y = NULL) + theme_v
ggsave(file.path(OUT_DIR, "04_reliability.png"), g4, width = 8, height = 4.5, dpi = 150, bg = "white")

# =============================================================================
# 5. Team-strength leak: are players getting credit for their team?
# =============================================================================
cat("5. TEAM-STRENGTH LEAK: how strongly does player GAR follow team quality?\n")
team_q <- if (DATA_SOURCE == "real") {
  qual %>% left_join(stand %>% select(team, team_quality = team_rating), by = "team")
} else qual %>% mutate(team_quality = team_strength_z)
if (any(!is.na(team_q$team_quality))) {
  leak <- team_q %>% group_by(position) %>%
    summarise(r_gar = cor(gar_per_90, team_quality, method = "spearman", use = "complete.obs"),
              r_naive = cor(naive, team_quality, method = "spearman", use = "complete.obs"), .groups = "drop")
  print(leak %>% mutate(across(where(is.double), ~ round(.x, 2))), n = Inf)
  lk <- mean(leak$r_gar)
  cat("   Some link is expected (good teams recruit good players); above ~0.5 suggests team credit leaking in.\n\n")
  add_result("5. Team-strength leak", "average rank correlation of player GAR with team quality", lk,
             mean(leak$r_naive), "Simple stat by position",
             if (lk <= 0.35) "fine" else if (lk <= 0.5) "watch" else "GAR may be crediting the team",
             "Lower is better for judging players who will change teams")
} else cat("   Skipped: no team-quality measure.\n\n")

# Face validity: how much is GAR just xG + xA?
fv <- qual %>% group_by(position) %>%
  summarise(r_with_xg_xa = cor(gar_per_90, xgxa_90, method = "spearman", use = "complete.obs"), .groups = "drop")
cat("   Rank correlation of GAR per 90 with plain xG + xA per 90 (near 1 = GAR adds little for that position):\n")
print(fv %>% mutate(r_with_xg_xa = round(r_with_xg_xa, 2)), n = Inf); cat("\n")

# =============================================================================
# 6. Sensitivity: do rankings survive other reasonable settings?
# =============================================================================
cat("6. SENSITIVITY: re-running player_gar.R with other settings\n")
if (RUN_SENSITIVITY && file.exists("player_gar.R")) {
  src <- readLines("player_gar.R")
  anchor <- grep("^# Optional overrides from the command line", src)[1]
  variants <- list(
    "Passer share 25%"            = "PASSER_SHARE <- 0.25",
    "Passer share 55%"            = "PASSER_SHARE <- 0.55",
    "Less shrinkage (135 min)"    = "PRIOR_MIN <- 135",
    "More shrinkage (540 min)"    = "PRIOR_MIN <- 540",
    "Replacement = 10th pct"      = "REPL_PCTL <- 0.10",
    "Replacement = 30th pct"      = "REPL_PCTL <- 0.30",
    "Manual defense weights"      = "WEIGHT_MODE <- 'manual'")
  method_change <- "Manual defense weights"   # a different model, not a small tweak
  sens <- list()
  for (nm in names(variants)) {
    tag <- gsub("[^a-z0-9]+", "_", tolower(nm)); od <- file.path(OUT_DIR, paste0("tmp_", tag))
    lines <- append(src, c(sprintf("DATA_SOURCE <- '%s'", DATA_SOURCE), variants[[nm]],
                           sprintf("OUT_DIR <- '%s'", od)), after = anchor - 1)
    # the DATA_SOURCE line must come before the real/simulated block, so set it at the top too
    lines <- sub('^DATA_SOURCE  <- ".*"', sprintf('DATA_SOURCE  <- "%s"', DATA_SOURCE), lines)
    tf <- sprintf("tmp_validate_%s.R", tag); writeLines(lines, tf)
    cat(sprintf("   %-28s ... ", nm)); t0 <- Sys.time()
    ok <- system2("Rscript", tf, stdout = FALSE, stderr = FALSE) == 0
    file.remove(tf)
    if (ok && file.exists(file.path(od, "player_gar.csv"))) {
      v <- read_csv(file.path(od, "player_gar.csv"), show_col_types = FALSE) %>%
        filter(season_year == LATEST) %>% select(player_id, gar_v = gar_per_90)
      j <- qual %>% select(player_id, gar_per_90) %>% inner_join(v, by = "player_id")
      top_b <- head(j$player_id[order(-j$gar_per_90)], 50); top_v <- head(j$player_id[order(-j$gar_v)], 50)
      sens[[nm]] <- tibble(variant = nm, spearman = cor(j$gar_per_90, j$gar_v, method = "spearman"),
                           top50_overlap = length(intersect(top_b, top_v)) / 50)
      cat(sprintf("rank r = %.2f, top-50 overlap %d%% (%.0fs)\n", sens[[nm]]$spearman,
                  round(100 * sens[[nm]]$top50_overlap), as.numeric(Sys.time() - t0, units = "secs")))
    } else cat("failed (skipped)\n")
    unlink(od, recursive = TRUE)
  }
  sens <- bind_rows(sens)
  if (nrow(sens)) {
    tw <- filter(sens, !variant %in% method_change); mc <- filter(sens, variant %in% method_change)
    if (nrow(tw)) add_result("6. Sensitivity", "lowest rank correlation after small setting tweaks", min(tw$spearman), NA, NA,
               if (min(tw$spearman) >= 0.9) "robust" else if (min(tw$spearman) >= 0.8) "mostly robust" else "rankings depend on settings",
               sprintf("worst: %s; lowest top-50 overlap %d%%", tw$variant[which.min(tw$spearman)],
                       round(100 * min(tw$top50_overlap))))
    if (nrow(mc)) add_result("6. Sensitivity", "rank correlation with hand-set (manual) defense weights", mc$spearman[1], NA, NA,
               if (mc$spearman[1] >= 0.9) "robust" else if (mc$spearman[1] >= 0.8) "mostly robust" else "defense depends on how weights are set",
               sprintf("top-50 overlap %d%%; the fitted defensive weights drive many rankings", round(100 * mc$top50_overlap[1])))
    g6 <- sens %>% pivot_longer(c(spearman, top50_overlap), names_to = "m", values_to = "v") %>%
      mutate(m = recode(m, spearman = "Rank correlation (all qualified players)", top50_overlap = "Top-50 overlap")) %>%
      ggplot(aes(v, reorder(variant, v), fill = m)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      geom_vline(xintercept = 0.9, linetype = 2, colour = "grey60") +
      scale_fill_manual(values = c(MAROON, GREY), name = NULL) +
      scale_x_continuous(limits = c(0, 1)) +
      labs(title = "Do the rankings survive other reasonable settings?",
           subtitle = "Each bar compares a re-run of player_gar.R with your current results (1 = identical)",
           x = NULL, y = NULL) + theme_v
    ggsave(file.path(OUT_DIR, "06_sensitivity.png"), g6, width = 8, height = 4.8, dpi = 150, bg = "white")
  }
  cat("\n")
} else cat("   Skipped (RUN_SENSITIVITY is FALSE).\n\n")

# =============================================================================
# 7. Year to year (needs two seasons)
# =============================================================================
cat("7. YEAR TO YEAR: does a player's GAR repeat the next season?\n")
yy <- pl %>% filter(minutes_played >= QUAL_MIN) %>%
  select(player_id, season_year, gar_per_90, naive) %>%
  inner_join(pl %>% filter(minutes_played >= QUAL_MIN) %>%
               transmute(player_id, season_year = season_year - 1, next_gar = gar_per_90, next_naive = naive),
             by = c("player_id", "season_year"))
if (nrow(yy) >= 30) {
  r_yy <- cor(yy$gar_per_90, yy$next_gar); r_yy_n <- cor(yy$naive, yy$next_naive)
  r_pred <- cor(yy$gar_per_90, yy$next_gar); r_pred_n <- cor(yy$naive, yy$next_gar)
  cat(sprintf("   %d returning players: GAR repeats at r = %.2f (simple stat repeats at %.2f)\n", nrow(yy), r_yy, r_yy_n))
  cat(sprintf("   Predicting next season's GAR: this season's GAR r = %.2f vs simple stat r = %.2f\n\n", r_pred, r_pred_n))
  add_result("7. Year to year", "r between a player's GAR in back-to-back seasons", r_yy, r_yy_n, "Simple stat by position",
             if (r_yy >= 0.5) "stable" else if (r_yy >= 0.3) "somewhat stable" else "mostly noise",
             "Does it hold up next year? (Simple stats can repeat more just because they reflect role, not quality)")
} else {
  cat("   Not enough returning players yet (needs a second season of data).\n\n")
  add_result("7. Year to year", "r between back-to-back seasons", NA, NA, NA, "needs a second season",
             "Re-run once 2027 data (or 2025 data in the same format) is added")
}

# =============================================================================
# 8. Awards (optional)
# =============================================================================
cat("8. AWARDS: do honored players rate high?\n")
if (!file.exists(AWARDS_FILE)) {
  write_csv(tibble(player_name = character(), team = character(), award = character()),
            "awards_template.csv")
  cat(sprintf("   No %s. Fill in awards_template.csv (player_name and team as in player_gar.csv,\n", AWARDS_FILE),
      "  e.g. 'All-ACC First Team') and save it as awards.csv.\n\n")
} else {
  aw <- read_csv(AWARDS_FILE, show_col_types = FALSE) %>% distinct(player_name, team) %>% mutate(honored = TRUE)
  d8 <- qual %>% left_join(aw, by = c("player_name", "team")) %>% mutate(honored = coalesce(honored, FALSE))
  auc <- function(score, y) { r <- rank(score); n1 <- sum(y); n0 <- sum(!y); (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
  # within position: compare honored players with others at the same position
  a_g <- d8 %>% group_by(position) %>% filter(any(honored), any(!honored)) %>%
    summarise(n = sum(honored), auc_gar = auc(gar_per_90, honored), auc_naive = auc(naive, honored), .groups = "drop")
  print(a_g %>% mutate(across(where(is.double), ~ round(.x, 2))), n = Inf)
  ag <- weighted.mean(a_g$auc_gar, a_g$n); an <- weighted.mean(a_g$auc_naive, a_g$n)
  cat(sprintf("   Matched %d honored players. AUC: GAR %.2f vs simple stat %.2f (0.5 = random, 1 = perfect)\n\n",
              sum(d8$honored), ag, an))
  add_result("8. Awards", "AUC: honored players rated above others at their position", ag, an, "Simple stat by position",
             if (ag >= an + 0.02) "GAR beats raw stats" else if (ag >= an - 0.02) "about equal" else "raw stats do better",
             "Coaches' and voters' judgment")
}

# =============================================================================
# 9. True talent (simulated data only)
# =============================================================================
if (any(!is.na(qual$true_talent))) {
  cat("9. TRUE TALENT (simulated answer key)\n")
  tt <- qual %>% group_by(position) %>%
    summarise(r_gar = cor(gar_per_90, true_talent), r_naive = cor(naive, true_talent), .groups = "drop")
  print(tt %>% mutate(across(where(is.double), ~ round(.x, 2))), n = Inf)
  add_result("9. True talent", "average r with the simulation's true talent", mean(tt$r_gar), mean(tt$r_naive),
             "Simple stat by position", if (mean(tt$r_gar) >= mean(tt$r_naive)) "GAR beats raw stats" else "raw stats do better",
             "Only possible with simulated data")
  cat("\n")
}

# ---- Summary ----------------------------------------------------------------
summ <- bind_rows(summary_rows)
write_csv(summ, file.path(OUT_DIR, "validation_summary.csv"), na = "")
cat("==================== SUMMARY ====================\n")
for (i in seq_len(nrow(summ))) {
  s <- summ[i, ]
  cat(sprintf("%-26s %-62s GAR %6s%s\n   -> %s. %s\n", s$check, s$measure,
              ifelse(is.na(s$gar), "—", sprintf("%.2f", s$gar)),
              ifelse(is.na(s$baseline), "", sprintf("  vs %s %.2f", s$baseline_name, s$baseline)),
              s$verdict, s$meaning))
}
cat(sprintf("\nCharts and validation_summary.csv are in %s/\n", OUT_DIR))
