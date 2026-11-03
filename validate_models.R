# =============================================================================
# validate_models.R: are GAR and PAR measuring something real, and which is better?
#
# Validates each model on its own AND compares them side by side. Run AFTER
# player_gar.R and player_war.R (with OUTCOME <- "points" for PAR).
#
# Each check asks a question the models were NOT fitted to, and compares them with
# a simple baseline (raw goal difference, xG + xA per 90, ...). A useful model
# should beat the baselines. Checks 1-5 run for every season in the data, and
# checks 1-2 also for all seasons pooled (more teams, tighter intervals).
#
#   1. Out-of-sample games   Does team rating from conference games predict
#                            NON-conference results (games not in the data)?
#   2. Outside judgment      Does team rating line up with Massey ratings?
#   3. Coaches' choices      Within a team and position, does the higher-rated
#                            player get more minutes?
#   4. Reliability           How much of the spread is signal vs sampling noise?
#                            (GAR only: PAR has no uncertainty ranges)
#   5. Team-strength leak    Is the model crediting players for their team?
#   6. Sensitivity           Do rankings survive other reasonable settings, and
#                            weights learned from one season only?
#                            (re-runs the model scripts; ~1 minute per run)
#   7. Year to year          Does a player's rating repeat next season? (2+ seasons)
#                            All returning players, and transfers on their own.
#   8. Awards (optional)     Do All-Conference / All-American players rate high?
#  10. GAR vs PAR            Where do the two models agree and disagree?
#  11. Next-season forecast  Do last season's ratings of THIS season's roster
#                            predict this season's results? (2+ seasons)
#
# Outputs (validation_output/):
#   validation_results.csv   every result, one row per check x model x season (long)
#   validation_compare.csv   the main results side by side: GAR | PAR | baseline
#   charts 00-08, validation_report.txt is what you see printed
#
# Run:  Rscript validate_models.R             (both models)
#       Rscript validate_models.R GAR         (just one)
# Packages: readr, dplyr, tidyr, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---- Settings ---------------------------------------------------------------
MODELS          <- c("GAR", "PAR")   # which models to validate: "GAR", "PAR" or both
PLAYERS_FILE    <- "real_player_metrics.csv"
STANDINGS_FILE  <- "real_standings.csv"
RPI_FILE        <- "rpi_standings_all_fixed.csv"   # overall records, for non-conference games
QUAL_MIN        <- 360               # minutes to count as a qualified player
PRIOR_MIN       <- 270               # same small-sample shrinkage as the models
RUN_SENSITIVITY <- TRUE              # re-run the model scripts with other settings (slow)
N_BOOT          <- 1000              # bootstrap samples for confidence intervals
OUT_DIR         <- "validation_output"
AWARDS_FILE     <- "awards.csv"      # optional: player_name, team, award
FOCUS_TEAM      <- "Virginia Tech"

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) MODELS <- intersect(toupper(args), c("GAR", "PAR"))
if (!length(MODELS)) stop("MODELS must include \"GAR\" and/or \"PAR\".")

# How to read each model's output. Every model gives a player rating per 90 and a
# team rating per game ABOVE AVERAGE (so teams can be compared).
MODEL_INFO <- list(
  GAR = list(script = "player_gar.R",
             dir = "GAR_output_real",
             player_file = "player_gar.csv", team_file = "team_gar.csv",
             rating = "gar_per_90", se = "se_per_90",
             team_rating = function(t) (t$team_off_gaa + t$team_def_gaa) / t$games,
             unit = "goals"),
  PAR = list(script = "player_war.R",
             dir = "par_output_real",
             player_file = "player_par.csv", team_file = "team_par.csv",
             rating = "par_per_90", se = "se_per_90",
             team_rating = function(t) t$team_paa / t$games,
             unit = "points")
)
# Settings each model is re-run with in check 6. method_change = a different model,
# not a small tweak, so it's reported separately. "Weights from <season> only" fits the
# weights on one season and rates every season with them (added automatically when the
# data has 2+ seasons): the latest season's results then come from weights that never
# saw its games.
SENS <- list(
  GAR = list("Passer share 25%"         = "PASSER_SHARE <- 0.25",
             "Passer share 55%"         = "PASSER_SHARE <- 0.55",
             "Less shrinkage (135 min)" = "PRIOR_MIN <- 135",
             "More shrinkage (540 min)" = "PRIOR_MIN <- 540",
             "Replacement = 10th pct"   = "REPL_PCTL <- 0.10",
             "Replacement = 30th pct"   = "REPL_PCTL <- 0.30",
             "Manual weights"           = "WEIGHT_MODE <- 'manual'"),
  PAR = list("Less shrinkage (135 min)" = "PRIOR_MIN <- 135",
             "More shrinkage (540 min)" = "PRIOR_MIN <- 540",
             "Replacement = 10th pct"   = "REPL_PCTL <- 0.10",
             "Replacement = 30th pct"   = "REPL_PCTL <- 0.30",
             "Scale on 10th-90th pct"   = "SCALE_LO <- 0.10; SCALE_HI <- 0.90",
             "Capped at 95th pct"       = "CAP_SCALE <- TRUE",
             "Points target (old)"      = "TARGET <- 'outcome'",
             "One weight per stat (old)" = "WEIGHTS_FILE <- 'war_weights_real.csv'")
)
METHOD_CHANGE <- c("Manual weights", "Points target (old)", "One weight per stat (old)")

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE)
COL <- c(GAR = "#861F41", PAR = "#2F6DB5", Baseline = "#B9B3AE")
theme_v <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        plot.title.position = "plot", legend.position = "top", legend.justification = "left")
save_chart <- function(p, name, w = 8, h = 5) ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 150, bg = "white")

results <- list()
SEASON_NOW <- NA   # season label stored with each result (set as the checks run)
add_result <- function(check, measure, model, value, baseline = NA, baseline_name = NA, verdict = NA, meaning = NA,
                       season = SEASON_NOW) {
  results[[length(results) + 1]] <<- tibble(check, measure, season = as.character(season), model, value = round(value, 3),
                                            baseline = round(baseline, 3), baseline_name, verdict, meaning)
}
beat_verdict <- function(v, b, tol = 0.03)
  if (is.na(v) || is.na(b)) NA else if (v >= b + tol) "beats the baseline" else if (v >= b - tol) "about equal" else "baseline does better"
boot_ci <- function(x, y, f = function(a, b) cor(a, b), n = N_BOOT) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  b <- replicate(n, { i <- sample(seq_along(x), replace = TRUE); f(x[i], y[i]) })
  quantile(b, c(0.05, 0.95), na.rm = TRUE)
}
# Same small-sample shrinkage the models use, so baselines get a fair comparison
shrink <- function(x, minutes, group) {
  avg <- ave(seq_along(x), group, FUN = function(i) {
    q <- i[minutes[i] >= QUAL_MIN & !is.na(x[i])]
    rep(if (length(q)) weighted.mean(x[q], minutes[q]) else mean(x[i], na.rm = TRUE), length(i))
  })
  x <- coalesce(x, avg)
  (x * minutes + avg * PRIOR_MIN) / (minutes + PRIOR_MIN)
}
z0 <- function(x) coalesce(as.numeric(x), 0)
col_or_na <- function(df, nm) if (nm %in% names(df)) as.numeric(df[[nm]]) else rep(NA_real_, nrow(df))
POS_ORDER <- c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")

# ---- Load -------------------------------------------------------------------
for (f in c(PLAYERS_FILE, STANDINGS_FILE)) if (!file.exists(f)) stop("Missing ", f)
mets  <- read_csv(PLAYERS_FILE, show_col_types = FALSE)
stand <- read_csv(STANDINGS_FILE, show_col_types = FALSE)

pl <- list(); tm <- list()
for (m in MODELS) {
  info <- MODEL_INFO[[m]]
  pf <- file.path(info$dir, info$player_file); tf <- file.path(info$dir, info$team_file)
  if (!file.exists(pf) || !file.exists(tf)) {
    warning(sprintf("%s skipped: %s not found. Run %s first%s.", m, pf, info$script,
                    if (m == "PAR") ' with OUTCOME <- "points"' else ""))
    next
  }
  p <- read_csv(pf, show_col_types = FALSE)
  pl[[m]] <- p %>% transmute(season_year, player_id, player_name, team, position, minutes_played,
                             rating = .data[[info$rating]],
                             se = if (!is.na(info$se) && info$se %in% names(p)) .data[[info$se]] else NA_real_)
  t <- read_csv(tf, show_col_types = FALSE)
  tm[[m]] <- t %>% transmute(season_year, team, team_rating = info$team_rating(t))
}
MODELS <- names(pl)
if (!length(MODELS)) stop("No model outputs found.")
# Models whose weights were fitted to Massey ratings (model_settings.csv, TARGET).
# Their Massey checks aren't independent, and Massey also uses non-conference games, so
# their check 1 is partly in-sample too; the honest team test for them is 6b.
FIT_TO_MASSEY <- MODELS[vapply(MODELS, function(m) {
  f <- file.path(MODEL_INFO[[m]]$dir, "model_settings.csv")
  file.exists(f) && identical(read_csv(f, show_col_types = FALSE) %>% filter(setting == "TARGET") %>% pull(value), "massey")
}, logical(1))]
massey_note <- function(m, verdict) if (m %in% FIT_TO_MASSEY) "not independent: fitted to Massey ratings" else verdict
LATEST  <- max(pl[[1]]$season_year)
SEASONS <- sort(unique(pl[[1]]$season_year))
CHECK_SEASONS <- SEASONS
POOLED <- if (length(CHECK_SEASONS) > 1) sprintf("%d-%d pooled", min(CHECK_SEASONS), max(CHECK_SEASONS)) else NA

# Simple per-90 baselines anyone could compute without a model: xG + xA for attackers,
# successful defensive actions for defenders, goals prevented for keepers
base <- mets %>%
  transmute(season_year, player_id, position, minutes_played,
            xgxa_90 = z0(col_or_na(mets, "xG / 90")) + z0(col_or_na(mets, "xA / 90")),
            defact_90 = col_or_na(mets, "Successful defensive actions / 90"),
            prev_90 = col_or_na(mets, "Prevented goals / 90")) %>%
  group_by(season_year) %>%
  mutate(naive_raw = case_when(position == "GK" & !all(is.na(prev_90)) ~ prev_90,
                               position %in% c("CB", "FB", "DM") ~ defact_90,
                               TRUE ~ xgxa_90),
         naive = shrink(naive_raw, minutes_played, paste(season_year, position))) %>%
  ungroup() %>%
  select(season_year, player_id, naive, xgxa_90)
pl <- lapply(pl, function(p) left_join(p, base, by = c("season_year", "player_id")))
qual <- lapply(pl, function(p) filter(p, season_year == LATEST, minutes_played >= QUAL_MIN))

# Team baselines from the standings (independent of either model), one season at a time
team_table <- function(yr) {
  st <- stand %>% filter(season_year == yr)
  tt <- st %>%
    mutate(games = coalesce(col_or_na(st, "games"), overall_w + overall_l + overall_t),
           gd_pg = (goals_for - goals_against) / games,
           ppg = (3 * overall_w + overall_t) / (overall_w + overall_l + overall_t))
  ms <- mets %>% filter(season_year == yr)
  xg_team <- ms %>%
    mutate(xga = if_else(position == "GK", z0(col_or_na(ms, "xG against, total")), 0)) %>%
    group_by(team) %>% summarise(xg_for = sum(z0(`xG, Total`)), xg_against = sum(xga), .groups = "drop")
  tt <- tt %>% left_join(xg_team, by = "team") %>% mutate(xgd_pg = (xg_for - xg_against) / games)
  for (m in MODELS) tt <- tt %>%
    left_join(tm[[m]] %>% filter(season_year == yr) %>% select(team, !!paste0("r_", m) := team_rating), by = "team")
  tt
}
# Non-conference record from the RPI file (games not in the data). In 2025 the RPI record
# includes the postseason, a few games of which are in the Wyscout data for deep-run teams.
nonconf <- function(yr) {
  if (!file.exists(RPI_FILE)) return(NULL)
  read_csv(RPI_FILE, show_col_types = FALSE) %>% filter(season_year == yr) %>%
    transmute(team, nc_w = overall_w - conf_w, nc_l = overall_l - conf_l, nc_t = overall_t - conf_t,
              nc_g = nc_w + nc_l + nc_t, nc_ppg = (3 * nc_w + nc_t) / nc_g)
}

cat(sprintf("Validating %s for season%s %s; players with %d+ minutes count as qualified\n\n",
            paste(MODELS, collapse = " and "), if (length(CHECK_SEASONS) > 1) "s" else "",
            paste(CHECK_SEASONS, collapse = " and "), QUAL_MIN))

# =============================================================================
# Checks 1-5, one season at a time
# =============================================================================
t1_all <- list(); teams_all <- list(); conc_latest <- NULL; r1_latest <- NULL
for (yr in CHECK_SEASONS) {
  SEASON_NOW <- yr
  teams <- team_table(yr)
  qual_y <- lapply(pl, function(p) filter(p, season_year == yr, minutes_played >= QUAL_MIN))
  teams_all[[as.character(yr)]] <- teams
  cat(sprintf("==================== SEASON %d ====================\n\n", yr))

  # ---- 1. Out-of-sample games: non-conference results ------------------------
  cat("1. OUT-OF-SAMPLE GAMES: does a conference-game team rating predict non-conference points per game?\n")
  rpi <- nonconf(yr)
  if (!is.null(rpi)) {
    t1 <- teams %>% inner_join(rpi, by = "team") %>% filter(nc_g >= 4)
    t1_all[[as.character(yr)]] <- t1
    preds <- c(setNames(paste0("r_", MODELS), paste("Team", MODELS, "per game")),
               "Conference goal difference per game" = "gd_pg",
               "Conference xG difference per game" = "xgd_pg",
               "Conference points per game" = "ppg")
    preds <- preds[preds %in% names(t1)]
    r1 <- tibble(predictor = names(preds), col = unname(preds)) %>% rowwise() %>%
      mutate(r = cor(t1[[col]], t1$nc_ppg, use = "complete.obs"),
             ci = list(boot_ci(t1[[col]], t1$nc_ppg)), lo = ci[1], hi = ci[2]) %>%
      ungroup() %>% select(-ci) %>%
      mutate(kind = case_when(col == "r_GAR" ~ "GAR", col == "r_PAR" ~ "PAR", TRUE ~ "Baseline"), season = yr)
    print(r1 %>% select(predictor, r, lo, hi) %>% mutate(across(r:hi, ~ round(.x, 2))), n = Inf)
    cat(sprintf("   %d teams, about %.0f non-conference games each.\n", nrow(t1), mean(t1$nc_g)))
    best <- r1 %>% filter(kind == "Baseline") %>% slice_max(r, n = 1)
    m0 <- lm(nc_ppg ~ gd_pg, data = t1)
    for (m in MODELS) {
      rv <- r1$r[r1$kind == m]
      m1 <- lm(reformulate(c("gd_pg", paste0("r_", m)), "nc_ppg"), data = t1)
      inc <- summary(m1)$r.squared - summary(m0)$r.squared; p_inc <- anova(m0, m1)$`Pr(>F)`[2]
      cat(sprintf("   %s on top of goal difference: +%.3f R-squared (p = %.3f)\n", m, inc, p_inc))
      add_result("1. Non-conference games", "r with non-conference points per game", m, rv, best$r, best$predictor,
                 beat_verdict(rv, best$r), if (m %in% FIT_TO_MASSEY)
                   "Partly in-sample: the weights were fitted to Massey, which includes these games (see 6b)" else
                   "Games the model never saw: the most honest team-level test")
      add_result("1. Non-conference games", "extra R-squared over conference goal difference", m, inc, NA, NA,
                 if (p_inc < 0.05) "adds information" else "no clear extra information", sprintf("p = %.3f", p_inc))
    }
    if (length(MODELS) == 2) {
      # do the two models carry different information? fit both together
      mb <- lm(nc_ppg ~ gd_pg + r_GAR + r_PAR, data = t1)
      cat(sprintf("   GAR and PAR together: R-squared %.3f (GAR alone %.3f, PAR alone %.3f)\n",
                  summary(mb)$r.squared, summary(lm(nc_ppg ~ gd_pg + r_GAR, data = t1))$r.squared,
                  summary(lm(nc_ppg ~ gd_pg + r_PAR, data = t1))$r.squared))
    }
    cat("\n")
    if (yr == LATEST) r1_latest <- r1
  } else cat("   Skipped: needs the RPI file's overall and conference records (real data).\n\n")

  # ---- 2. Outside judgment: Massey ratings -----------------------------------
  cat("2. OUTSIDE JUDGMENT: does team rating line up with Massey ratings?\n")
  if ("team_rating" %in% names(teams) && any(!is.na(teams$team_rating))) {
    r_gd <- cor(teams$gd_pg, teams$team_rating, use = "complete.obs")
    for (m in MODELS) {
      rv <- cor(teams[[paste0("r_", m)]], teams$team_rating, use = "complete.obs")
      ci <- boot_ci(teams[[paste0("r_", m)]], teams$team_rating)
      cat(sprintf("   %s: r = %.2f (90%% CI %.2f to %.2f)%s\n", m, rv, ci[1], ci[2],
                  if (m %in% FIT_TO_MASSEY) " (fitted to Massey, not an independent check)" else ""))
      add_result("2. Massey ratings", "r with Massey team rating", m, rv, r_gd, "Conference goal difference per game",
                 massey_note(m, beat_verdict(rv, r_gd)), "Massey uses the whole season and opponents; partly the same games")
    }
    cat(sprintf("   Baseline, conference goal difference: r = %.2f\n\n", r_gd))
  } else cat("   Skipped: no Massey ratings in the standings file.\n\n")

  # ---- 3. Coaches' choices: playing time within team and position ------------
  cat("3. COACHES' CHOICES: within a team and position, does the higher-rated player play more?\n")
  pair_conc <- function(d, metric) {
    d %>% filter(season_year == yr, minutes_played >= 90, !is.na(.data[[metric]])) %>%
      group_by(team, position) %>% filter(n() >= 2) %>%
      group_modify(function(g, k) {
        cmb <- t(combn(nrow(g), 2))
        dm <- sign(g$minutes_played[cmb[, 1]] - g$minutes_played[cmb[, 2]])
        dv <- sign(g[[metric]][cmb[, 1]] - g[[metric]][cmb[, 2]])
        tibble(conc = sum(dm * dv > 0), disc = sum(dm * dv < 0))
      }) %>% ungroup() %>%
      group_by(position) %>% summarise(pairs = sum(conc + disc), concordance = sum(conc) / pairs, .groups = "drop")
  }
  conc <- bind_rows(c(lapply(MODELS, function(m) pair_conc(pl[[m]], "rating") %>% mutate(metric = m)),
                      list(pair_conc(pl[[1]], "naive") %>% mutate(metric = "Baseline"))))
  conc_all <- conc %>% group_by(metric) %>% summarise(concordance = weighted.mean(concordance, pairs))
  print(conc %>% select(-pairs) %>% pivot_wider(names_from = metric, values_from = concordance) %>%
          mutate(across(-position, ~ round(.x, 2))) %>% arrange(match(position, POS_ORDER)), n = Inf)
  cb <- conc_all$concordance[conc_all$metric == "Baseline"]
  for (m in MODELS) {
    cv <- conc_all$concordance[conc_all$metric == m]
    cat(sprintf("   %s overall: %.2f\n", m, cv))
    add_result("3. Coaches' choices", "share of teammate pairs where the higher-rated player plays more", m, cv, cb,
               "Simple stat by position", beat_verdict(cv, cb, 0.02), "0.50 = coin flip; coaches see every practice")
  }
  cat(sprintf("   Baseline (simple stat by position): %.2f. 0.50 = no better than a coin flip.\n\n", cb))
  if (yr == LATEST) conc_latest <- conc

  # ---- 4. Reliability (needs uncertainty ranges: GAR) -------------------------
  cat("4. RELIABILITY: how much of the real differences between players does the rating capture?\n")
  for (m in MODELS) {
    q <- qual_y[[m]]
    if (all(is.na(q$se))) {
      cat(sprintf("   %s: not available (no uncertainty ranges in its output)\n", m))
      add_result("4. Reliability", "share of real player differences captured (0-1)", m, NA, NA, NA,
                 "not available", "the model has no uncertainty ranges")
      next
    }
    # ratings are already shrunk, so: true spread = spread of ratings + average uncertainty
    rel <- q %>% group_by(position) %>%
      summarise(n = n(), reliability = var(rating) / (var(rating) + mean(se^2)), .groups = "drop")
    ra <- weighted.mean(rel$reliability, rel$n)
    cat(sprintf("   %s: %.2f overall (correlation with true level about %.2f). By position: %s\n", m, ra, sqrt(ra),
                paste(sprintf("%s %.2f", rel$position, rel$reliability)[order(match(rel$position, POS_ORDER))], collapse = ", ")))
    add_result("4. Reliability", "share of real player differences captured (0-1)", m, ra, NA, NA,
               if (ra >= 0.7) "good" else if (ra >= 0.5) "moderate: compare tiers, not exact ranks" else "low: rank in tiers",
               "sampling noise only, not model error, so a best case")
  }
  cat("\n")

  # ---- 5. Team-strength leak --------------------------------------------------
  cat("5. TEAM-STRENGTH LEAK: how strongly does player rating follow team quality?\n")
  team_q <- stand %>% filter(season_year == yr) %>% select(team, tq = team_rating)
  for (m in MODELS) {
    q <- qual_y[[m]]
    q <- left_join(q, team_q, by = "team")
    if (all(is.na(q$tq))) { cat("   Skipped: no team-quality measure.\n"); break }
    lk <- q %>% group_by(position) %>%
      summarise(r = cor(rating, tq, method = "spearman", use = "complete.obs"),
                rb = cor(naive, tq, method = "spearman", use = "complete.obs"), .groups = "drop")
    cat(sprintf("   %s: average %.2f (baseline %.2f)\n", m, mean(lk$r), mean(lk$rb)))
    add_result("5. Team-strength leak", "average rank correlation of player rating with team quality", m,
               mean(lk$r), mean(lk$rb), "Simple stat by position",
               if (mean(lk$r) <= 0.35) "fine" else if (mean(lk$r) <= 0.5) "watch" else "may be crediting the team",
               "lower is better for players who will change teams; some link is expected")
  }
  cat("   Some link is expected (good teams recruit good players); above ~0.5 suggests team credit leaking in.\n\n")
}

# ---- Checks 1-2, all seasons pooled ------------------------------------------
# Team ratings are per game above the season average, so team-seasons can be pooled.
if (!is.na(POOLED)) {
  SEASON_NOW <- POOLED
  cat(sprintf("==================== %s ====================\n\n", toupper(POOLED)))
  if (length(t1_all) > 1) {
    t1p <- bind_rows(t1_all, .id = "season")
    cat(sprintf("1. NON-CONFERENCE GAMES, %d team-seasons\n", nrow(t1p)))
    pp <- c(setNames(paste0("r_", MODELS), MODELS), "Conference goal difference per game" = "gd_pg",
            "Conference xG difference per game" = "xgd_pg", "Conference points per game" = "ppg")
    pp <- pp[pp %in% names(t1p)]
    rp <- sapply(pp, function(cl) cor(t1p[[cl]], t1p$nc_ppg, use = "complete.obs"))
    bb <- rp[!names(rp) %in% MODELS]; bname <- names(bb)[which.max(bb)]
    for (nm in names(rp)) {
      ci <- boot_ci(t1p[[pp[[nm]]]], t1p$nc_ppg)
      cat(sprintf("   %-36s r = %.2f (90%% CI %.2f to %.2f)\n", nm, rp[[nm]], ci[1], ci[2]))
    }
    m0 <- lm(nc_ppg ~ gd_pg + factor(season), data = t1p)
    for (m in MODELS) {
      add_result("1. Non-conference games", "r with non-conference points per game", m, rp[[m]], max(bb), bname,
                 beat_verdict(rp[[m]], max(bb)), "Games the model never saw: the most honest team-level test")
      m1 <- update(m0, as.formula(paste(". ~ . +", paste0("r_", m))))
      inc <- summary(m1)$r.squared - summary(m0)$r.squared; p_inc <- anova(m0, m1)$`Pr(>F)`[2]
      cat(sprintf("   %s on top of goal difference: +%.3f R-squared (p = %.4f)\n", m, inc, p_inc))
      add_result("1. Non-conference games", "extra R-squared over conference goal difference", m, inc, NA, NA,
                 if (p_inc < 0.05) "adds information" else "no clear extra information", sprintf("p = %.4f", p_inc))
    }
    cat("\n")
  }
  tp <- bind_rows(teams_all)
  if ("team_rating" %in% names(tp) && any(!is.na(tp$team_rating))) {
    cat(sprintf("2. MASSEY RATINGS, %d team-seasons (Massey scaled within each season)\n", nrow(tp)))
    tp <- tp %>% group_by(season_year) %>% mutate(massey_z = as.numeric(scale(team_rating))) %>% ungroup()
    r_gd <- cor(tp$gd_pg, tp$massey_z, use = "complete.obs")
    for (m in MODELS) {
      rv <- cor(tp[[paste0("r_", m)]], tp$massey_z, use = "complete.obs")
      cat(sprintf("   %s: r = %.2f\n", m, rv))
      add_result("2. Massey ratings", "r with Massey team rating", m, rv, r_gd, "Conference goal difference per game",
                 massey_note(m, beat_verdict(rv, r_gd)), "Massey uses the whole season and opponents; partly the same games")
    }
    cat(sprintf("   Baseline, conference goal difference: r = %.2f\n\n", r_gd))
  }
}
SEASON_NOW <- LATEST

# Charts for checks 1 and 3 (latest season)
if (!is.null(r1_latest)) {
  g1 <- ggplot(r1_latest, aes(r, reorder(predictor, r), fill = kind)) +
    geom_col(width = 0.6) + geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.2, orientation = "y") +
    scale_fill_manual(values = COL, guide = "none") +
    labs(title = "Which conference-game measure best predicts non-conference results?",
         subtitle = sprintf("Correlation with non-conference points per game (games not in the data), %d, 90%% intervals", LATEST),
         x = "Correlation (r)", y = NULL) + theme_v
  save_chart(g1, "01_nonconf_games.png", h = 3.8)
  sc <- t1_all[[as.character(LATEST)]] %>% select(team, nc_ppg, any_of(paste0("r_", MODELS))) %>%
    pivot_longer(starts_with("r_"), names_to = "model", values_to = "rating") %>%
    mutate(model = sub("^r_", "", model))
  g1b <- ggplot(sc, aes(rating, nc_ppg)) +
    geom_point(colour = "#B9B3AE", size = 1.8) +
    geom_point(data = filter(sc, team == FOCUS_TEAM), aes(colour = model), size = 3.5, show.legend = FALSE) +
    geom_smooth(aes(colour = model), method = "lm", se = FALSE, formula = y ~ x, show.legend = FALSE) +
    scale_colour_manual(values = COL) +
    facet_wrap(~ model, scales = "free_x") +
    labs(title = "Team rating from conference games vs non-conference points per game",
         subtitle = sprintf("Each dot is a team, %d; the large dot is %s", LATEST, FOCUS_TEAM),
         x = "Team rating per game (above average)", y = "Non-conference points per game") + theme_v
  save_chart(g1b, "01b_nonconf_scatter.png", w = 9, h = 4.5)
}
g3 <- ggplot(conc_latest, aes(concordance, factor(position, levels = rev(POS_ORDER)), colour = metric)) +
  geom_vline(xintercept = 0.5, linetype = 2, colour = "grey60") +
  geom_line(aes(group = position), colour = "grey85", linewidth = 2) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = COL, name = NULL) +
  labs(title = "Do coaches give more minutes to higher-rated players?",
       subtitle = sprintf("Share of same-team, same-position pairs where the higher-rated player played more, %d (0.5 = coin flip)", LATEST),
       x = "Agreement with coaches' playing-time choices", y = NULL) + theme_v
save_chart(g3, "02_coaches_choices.png", h = 4.8)

cat(sprintf("==================== ACROSS SEASONS ====================\n\n"))

# =============================================================================
# 6. Sensitivity
# =============================================================================
cat(sprintf("6. SENSITIVITY: re-running each model with other settings (rankings compared for %d)\n", LATEST))
if (length(SEASONS) > 1)
  for (m in names(SENS)) for (y in SEASONS)
    SENS[[m]][[sprintf("Weights from %d only", y)]] <-
      sprintf("FIT_EXCLUDE <- c(2020, %s)", paste(setdiff(SEASONS, y), collapse = ", "))
sens_all <- list(); sens_team <- list()
if (RUN_SENSITIVITY) {
  for (m in MODELS) {
    info <- MODEL_INFO[[m]]
    if (!file.exists(info$script)) { cat(sprintf("   %s: %s not found, skipped\n", m, info$script)); next }
    src <- readLines(info$script)
    anchor <- grep("^# Optional overrides from the command line", src)[1]
    if (m == "PAR") src <- sub('^OUTCOME <- ".*"', 'OUTCOME <- "points"', src)
    for (nm in names(SENS[[m]])) {
      tag <- paste0(tolower(m), "_", gsub("[^a-z0-9]+", "_", tolower(nm)))
      od <- file.path(OUT_DIR, paste0("tmp_", tag))
      lines <- append(src, c(SENS[[m]][[nm]], "MAKE_PLOTS <- FALSE", sprintf("OUT_DIR <- '%s'", od)), after = anchor - 1)
      tf <- sprintf("tmp_validate_%s.R", tag); writeLines(lines, tf)
      cat(sprintf("   %s %-26s ... ", m, nm)); t0 <- Sys.time()
      ok <- system2(file.path(R.home("bin"), "Rscript"), tf, stdout = FALSE, stderr = FALSE) == 0
      file.remove(tf)
      out <- file.path(od, info$player_file)
      if (ok && file.exists(out)) {
        v <- read_csv(out, show_col_types = FALSE) %>% filter(season_year == LATEST) %>%
          select(player_id, v = all_of(info$rating))
        j <- qual[[m]] %>% select(player_id, rating) %>% inner_join(v, by = "player_id")
        top_b <- head(j$player_id[order(-j$rating)], 50); top_v <- head(j$player_id[order(-j$v)], 50)
        sens_all[[length(sens_all) + 1]] <- tibble(model = m, variant = nm,
          spearman = cor(j$rating, j$v, method = "spearman"),
          top50_overlap = length(intersect(top_b, top_v)) / 50)
        cat(sprintf("rank r = %.2f, top-50 overlap %d%% (%.0fs)\n", tail(sens_all, 1)[[1]]$spearman,
                    round(100 * tail(sens_all, 1)[[1]]$top50_overlap), as.numeric(Sys.time() - t0, units = "secs")))
        # Weights from one season only: how well do they rate the OTHER season's teams?
        if (grepl("^Weights from [0-9]+ only$", nm)) {
          tv <- read_csv(file.path(od, info$team_file), show_col_types = FALSE)
          tv <- tv %>% transmute(season_year, team, r_v = info$team_rating(tv))
          sens_team[[length(sens_team) + 1]] <- tibble(model = m, fit_season = as.integer(gsub("\\D", "", nm)),
                                                       teams = list(tv))
        }
      } else cat("failed (skipped)\n")
      unlink(od, recursive = TRUE)
    }
  }
  sens <- bind_rows(sens_all)
  is_season_fit <- grepl("^Weights from [0-9]+ only$", sens$variant)
  for (m in intersect(MODELS, unique(sens$model))) {
    s <- filter(sens, model == m)
    tw <- filter(s, !variant %in% METHOD_CHANGE, !grepl("^Weights from", variant))
    mc <- filter(s, variant %in% METHOD_CHANGE); sf <- filter(s, grepl("^Weights from", variant))
    if (nrow(tw)) add_result("6. Sensitivity", "lowest rank correlation after small setting tweaks", m, min(tw$spearman), NA, NA,
      if (min(tw$spearman) >= 0.9) "robust" else if (min(tw$spearman) >= 0.8) "mostly robust" else "rankings depend on settings",
      sprintf("worst: %s; lowest top-50 overlap %d%%", tw$variant[which.min(tw$spearman)], round(100 * min(tw$top50_overlap))))
    for (i in seq_len(nrow(mc))) add_result("6. Sensitivity", sprintf("rank correlation with a different method: %s", tolower(mc$variant[i])),
      m, mc$spearman[i], NA, NA,
      if (mc$spearman[i] >= 0.9) "robust" else if (mc$spearman[i] >= 0.8) "mostly robust" else "depends on the method",
      sprintf("top-50 overlap %d%%", round(100 * mc$top50_overlap[i])))
    for (i in seq_len(nrow(sf))) add_result("6. Sensitivity", sprintf("rank correlation with %s", tolower(sf$variant[i])),
      m, sf$spearman[i], NA, NA,
      if (sf$spearman[i] >= 0.9) "robust" else if (sf$spearman[i] >= 0.8) "mostly robust" else "weights change between seasons",
      sprintf("top-50 overlap %d%%", round(100 * sf$top50_overlap[i])))
  }
  # Weights learned on one season, judged on the other season's non-conference games
  if (length(sens_team)) {
    cat("\n   Weights learned on ONE season, tested on the OTHER season's non-conference games:\n")
    for (k in seq_along(sens_team)) {
      st <- sens_team[[k]]; m <- st$model
      for (y in setdiff(CHECK_SEASONS, st$fit_season)) {
        t1 <- t1_all[[as.character(y)]]
        if (is.null(t1)) next
        jt <- t1 %>% inner_join(st$teams[[1]] %>% filter(season_year == y) %>% select(team, r_v), by = "team")
        rv <- cor(jt$r_v, jt$nc_ppg, use = "complete.obs"); r_all <- cor(jt[[paste0("r_", m)]], jt$nc_ppg, use = "complete.obs")
        cat(sprintf("   %s, weights from %d, rating %d teams: r = %.2f (weights from all seasons %.2f)\n",
                    m, st$fit_season, y, rv, r_all))
        add_result("6b. Weights from another season", sprintf("r with %d non-conference points per game, weights fitted on %d only", y, st$fit_season),
                   m, rv, r_all, "Weights fitted on all seasons", beat_verdict(rv, r_all),
                   "Fully out of sample: neither the weights nor the ratings saw these games", season = y)
      }
    }
  }
  if (nrow(sens)) {
    g6 <- sens %>% pivot_longer(c(spearman, top50_overlap), names_to = "m", values_to = "v") %>%
      mutate(m = recode(m, spearman = "Rank correlation (all qualified players)", top50_overlap = "Top-50 overlap")) %>%
      ggplot(aes(v, reorder(variant, v), fill = m)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      geom_vline(xintercept = 0.9, linetype = 2, colour = "grey60") +
      scale_fill_manual(values = c("#44546A", "#B9B3AE"), name = NULL) +
      scale_x_continuous(limits = c(0, 1)) +
      facet_wrap(~ model, ncol = 1, scales = "free_y") +
      labs(title = "Do the rankings survive other reasonable settings?",
           subtitle = sprintf("Each bar compares a re-run with the current %d results (1 = identical)", LATEST), x = NULL, y = NULL) + theme_v
    save_chart(g6, "04_sensitivity.png", h = 3 + 1.3 * length(unique(sens$model)) * 2)
  }
  cat("\n")
} else cat("   Skipped (RUN_SENSITIVITY is FALSE).\n\n")

# =============================================================================
# 7. Year to year
# =============================================================================
cat("7. YEAR TO YEAR: does a player's rating repeat the next season?\n")
# who changed teams between seasons (the id stays the same after a transfer)
moves <- mets %>% select(player_id, season_year, team) %>%
  inner_join(mets %>% transmute(player_id, season_year = season_year - 1, next_team = team),
             by = c("player_id", "season_year")) %>%
  transmute(player_id, season_year, moved = team != next_team)
yy_all <- list()
for (m in MODELS) {
  d <- pl[[m]] %>% filter(minutes_played >= QUAL_MIN) %>% select(player_id, season_year, position, rating, naive)
  yy <- d %>% inner_join(d %>% transmute(player_id, season_year = season_year - 1, next_r = rating, next_n = naive),
                         by = c("player_id", "season_year")) %>%
    left_join(moves, by = c("player_id", "season_year"))
  if (nrow(yy) >= 30) {
    # Within position (each player against his position's average, first-season position),
    # so the r isn't just "CBs rate higher than CMs". This matters most for the simple stat,
    # which is a different stat at each position.
    r_within <- function(a, b, g) cor(ave(a, g, FUN = function(x) x - mean(x)), ave(b, g, FUN = function(x) x - mean(x)))
    r_yy <- r_within(yy$rating, yy$next_r, yy$position); r_n <- r_within(yy$naive, yy$next_n, yy$position)
    cat(sprintf("   %s: r = %.2f within position over %d returning players (simple stat %.2f); across positions %.2f (simple stat %.2f)\n",
                m, r_yy, nrow(yy), r_n, cor(yy$rating, yy$next_r), cor(yy$naive, yy$next_n)))
    add_result("7. Year to year", "r between a player's rating in back-to-back seasons (within position)", m, r_yy, r_n,
               "Simple stat by position", if (r_yy >= 0.5) "stable" else if (r_yy >= 0.3) "somewhat stable" else "mostly noise",
               "Conference games only: ~5-12 games a season, so a lot of noise either way", season = POOLED)
    for (grp in c(FALSE, TRUE)) {
      g <- filter(yy, moved == grp)
      if (nrow(g) < 20) next
      rg <- r_within(g$rating, g$next_r, g$position); rn <- r_within(g$naive, g$next_n, g$position)
      lab <- if (grp) "transferred" else "stayed at the same team"
      cat(sprintf("      %s: r = %.2f (n = %d, simple stat %.2f)\n", lab, rg, nrow(g), rn))
      add_result("7. Year to year", sprintf("r between back-to-back seasons (within position), players who %s", lab), m, rg, rn,
                 "Simple stat by position", beat_verdict(rg, rn),
                 if (grp) "The recruiting question: does a rating carry over to a new team?" else "Same team, coach and role",
                 season = POOLED)
    }
    yy_all[[m]] <- yy %>% mutate(model = m)
  } else {
    cat(sprintf("   %s: needs a second season of data\n", m))
    add_result("7. Year to year", "r between back-to-back seasons", m, NA, NA, NA, "needs a second season", NA)
  }
}
if (length(yy_all)) {
  yy_p <- bind_rows(yy_all) %>% mutate(group = if_else(coalesce(moved, FALSE), "Transferred", "Same team"))
  g7 <- ggplot(yy_p, aes(rating, next_r)) +
    geom_hline(yintercept = 0, colour = "grey85") + geom_vline(xintercept = 0, colour = "grey85") +
    geom_point(aes(colour = group), alpha = 0.45, size = 1.4) +
    geom_smooth(aes(colour = group), method = "lm", se = FALSE, formula = y ~ x) +
    scale_colour_manual(values = c("Same team" = "#B9B3AE", "Transferred" = "#861F41"), name = NULL) +
    facet_wrap(~ model, scales = "free") +
    labs(title = "Does a player's rating carry over to the next season?",
         subtitle = sprintf("Players with %d+ minutes in both seasons; rating per 90", QUAL_MIN),
         x = "Rating, first season", y = "Rating, next season") + theme_v
  save_chart(g7, "06_year_to_year.png", w = 9, h = 4.8)
}
cat("\n")

# =============================================================================
# 11. Next-season forecast: last season's ratings of this season's roster
# =============================================================================
if (length(SEASONS) > 1) {
  cat(sprintf("11. NEXT-SEASON FORECAST: do %d ratings of the %d roster predict %d results?\n", LATEST - 1, LATEST, LATEST))
  prev_y <- LATEST - 1
  cur_t <- team_table(LATEST); prev_t <- team_table(prev_y)
  # 'roster' = this season's players and their minutes (who plays is known; how well isn't).
  # Each player gets his rating from last season; a player with no rating last season
  # (newcomer, freshman) gets the median of last season's low-minute players at his position.
  forecast <- function(rating_col, d_prev) {
    fill <- d_prev %>% filter(minutes_played < QUAL_MIN) %>% group_by(position) %>%
      summarise(fill = median(.data[[rating_col]], na.rm = TRUE), .groups = "drop")
    mets %>% filter(season_year == LATEST, minutes_played > 0) %>%
      select(team, player_id, position, minutes_played) %>%
      left_join(d_prev %>% select(player_id, prev = all_of(rating_col)), by = "player_id") %>%
      left_join(fill, by = "position") %>%
      mutate(known = !is.na(prev), r = coalesce(prev, fill)) %>%
      group_by(team) %>%
      summarise(f = sum(r * minutes_played, na.rm = TRUE) / sum(minutes_played) * 11,  # rating per player-slot x 11
                known_share = sum(minutes_played[known]) / sum(minutes_played), .groups = "drop")
  }
  f_all <- cur_t %>% select(team, gd_pg, ppg, team_rating) %>%
    left_join(prev_t %>% select(team, gd_prev = gd_pg, massey_prev = team_rating), by = "team")
  for (m in MODELS) {
    fm <- forecast("rating", pl[[m]] %>% filter(season_year == prev_y))
    f_all <- f_all %>% left_join(fm %>% select(team, !!paste0("f_", m) := f), by = "team")
    if (m == MODELS[1]) f_all <- f_all %>% left_join(fm %>% select(team, known_share), by = "team")
  }
  # the simple stat is a different stat at each position, so put it on one scale first
  # (z-score within position, qualified players' spread)
  fb <- forecast("naive_z", base %>% filter(season_year == prev_y) %>%
                   left_join(mets %>% filter(season_year == prev_y) %>% select(player_id, position, minutes_played), by = "player_id") %>%
                   group_by(position) %>%
                   mutate(naive_z = (naive - mean(naive[minutes_played >= QUAL_MIN], na.rm = TRUE)) /
                            sd(naive[minutes_played >= QUAL_MIN], na.rm = TRUE)) %>% ungroup())
  f_all <- f_all %>% left_join(fb %>% select(team, f_base = f), by = "team") %>%
    filter(!is.na(gd_prev))
  cat(sprintf("   %d teams in both seasons; on average %.0f%% of this season's minutes go to players rated last season\n",
              nrow(f_all), 100 * mean(f_all$known_share, na.rm = TRUE)))
  targets <- c("goal difference per game" = "gd_pg", "Massey rating" = "team_rating", "points per game" = "ppg")
  targets <- targets[targets %in% names(f_all) & sapply(targets, function(x) any(!is.na(f_all[[x]])))]
  fc_rows <- list()
  for (tn in names(targets)) {
    y <- f_all[[targets[[tn]]]]
    rb_prev <- cor(f_all$gd_prev, y, use = "complete.obs")
    rb_mass <- if (any(!is.na(f_all$massey_prev))) cor(f_all$massey_prev, y, use = "complete.obs") else NA
    rb_naive <- cor(f_all$f_base, y, use = "complete.obs")
    best_b <- max(c(rb_prev, rb_mass, rb_naive), na.rm = TRUE)
    best_n <- c("Last season's goal difference", "Last season's Massey rating", "Roster's simple stat")[
      which.max(c(rb_prev, coalesce(rb_mass, -Inf), rb_naive))]
    cat(sprintf("   %d %s: last season's GD r = %.2f, last season's Massey r = %.2f, roster simple stat r = %.2f\n",
                LATEST, tn, rb_prev, rb_mass, rb_naive))
    fc_rows[[length(fc_rows) + 1]] <- tibble(target = tn, who = c("Last season's goal difference", "Last season's Massey rating", "Roster's simple stat"),
                                             kind = "Baseline", r = c(rb_prev, rb_mass, rb_naive))
    for (m in MODELS) {
      rv <- cor(f_all[[paste0("f_", m)]], y, use = "complete.obs")
      # does the roster rating add anything to last season's team results?
      ok <- complete.cases(f_all[[paste0("f_", m)]], y, f_all$gd_prev, f_all$massey_prev)
      d0 <- data.frame(y = y, gd = f_all$gd_prev, ms = f_all$massey_prev, f = f_all[[paste0("f_", m)]])[ok, ]
      m0 <- lm(y ~ gd + ms, d0); m1 <- lm(y ~ gd + ms + f, d0)
      inc <- summary(m1)$r.squared - summary(m0)$r.squared; p_inc <- anova(m0, m1)$`Pr(>F)`[2]
      cat(sprintf("      %s roster rating r = %.2f; on top of last season's GD + Massey: +%.3f R-squared (p = %.4f)\n",
                  m, rv, inc, p_inc))
      add_result("11. Next-season forecast", sprintf("r with %d %s", LATEST, tn), m, rv, best_b, best_n,
                 if (tn == "Massey rating") massey_note(m, beat_verdict(rv, best_b)) else beat_verdict(rv, best_b), "Last season's ratings of this season's roster: what a recruiting class adds",
                 season = LATEST)
      add_result("11. Next-season forecast", sprintf("extra R-squared for %d %s over last season's GD + Massey", LATEST, tn),
                 m, inc, NA, NA, if (p_inc < 0.05) "adds information" else "no clear extra information",
                 sprintf("p = %.4f", p_inc), season = LATEST)
      fc_rows[[length(fc_rows) + 1]] <- tibble(target = tn, who = paste("Roster", m), kind = m, r = rv)
    }
  }
  fc <- bind_rows(fc_rows) %>% filter(!is.na(r))
  g11 <- ggplot(fc, aes(r, reorder(who, r), fill = kind)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = sprintf("%.2f", r)), hjust = -0.15, size = 3.3) +
    scale_fill_manual(values = COL, guide = "none") +
    scale_x_continuous(limits = c(min(0, fc$r) - 0.05, 1)) +
    facet_wrap(~ target, ncol = 1, scales = "free_y") +
    labs(title = sprintf("Predicting %d before it starts", LATEST),
         subtitle = sprintf("Correlation with %d results. Roster = %d players, rated on their %d games", LATEST, LATEST, prev_y),
         x = "Correlation (r)", y = NULL) + theme_v
  save_chart(g11, "07_next_season_forecast.png", h = 2 + 1.6 * length(unique(fc$target)))
  write_csv(f_all, file.path(OUT_DIR, "next_season_forecast_teams.csv"), na = "")
  cat("\n")
}

# =============================================================================
# 8. Awards (optional)
# =============================================================================
cat("8. AWARDS: do honored players rate high?\n")
if (!file.exists(AWARDS_FILE)) {
  if (!file.exists("awards_template.csv"))
    write_csv(tibble(player_name = character(), team = character(), award = character()), "awards_template.csv")
  cat(sprintf("   No %s. Fill in awards_template.csv (player_name and team as in the model output) and save it as %s.\n\n",
              AWARDS_FILE, AWARDS_FILE))
} else {
  aw <- read_csv(AWARDS_FILE, show_col_types = FALSE) %>% distinct(player_name, team) %>% mutate(honored = TRUE)
  auc <- function(score, y) { r <- rank(score); n1 <- sum(y); n0 <- sum(!y); (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
  for (m in MODELS) {
    d8 <- qual[[m]] %>% left_join(aw, by = c("player_name", "team")) %>% mutate(honored = coalesce(honored, FALSE))
    a <- d8 %>% group_by(position) %>% filter(any(honored), any(!honored)) %>%
      summarise(n = sum(honored), a_m = auc(rating, honored), a_b = auc(naive, honored), .groups = "drop")
    if (!nrow(a)) { cat(sprintf("   %s: no honored players matched\n", m)); next }
    am <- weighted.mean(a$a_m, a$n); ab <- weighted.mean(a$a_b, a$n)
    cat(sprintf("   %s: AUC %.2f vs simple stat %.2f (%d honored players matched)\n", m, am, ab, sum(d8$honored)))
    add_result("8. Awards", "AUC: honored players rated above others at their position", m, am, ab,
               "Simple stat by position", beat_verdict(am, ab, 0.02), "0.5 = random, 1 = perfect")
  }
  cat("\n")
}

# =============================================================================
# 10. GAR vs PAR: where they agree
# =============================================================================
if (all(c("GAR", "PAR") %in% MODELS)) {
  cat(sprintf("10. GAR vs PAR: how similarly do they rank players at each position? (%d)\n", LATEST))
  both <- qual$GAR %>% select(player_id, player_name, team, position, minutes_played, gar = rating) %>%
    inner_join(qual$PAR %>% select(player_id, par = rating), by = "player_id") %>%
    group_by(position) %>% mutate(rank_gar = rank(-gar), rank_par = rank(-par), n = n()) %>% ungroup()
  agree <- both %>% group_by(position) %>%
    summarise(n = n(), rank_r = cor(gar, par, method = "spearman"),
              top10_shared = length(intersect(player_id[rank_gar <= 10], player_id[rank_par <= 10])) / 10,
              .groups = "drop") %>% arrange(match(position, POS_ORDER))
  print(agree %>% mutate(across(where(is.double), ~ round(.x, 2))), n = Inf)
  add_result("10. GAR vs PAR", "rank correlation between GAR and PAR (within position, average)", "both",
             mean(agree$rank_r), NA, NA,
             if (mean(agree$rank_r) >= 0.85) "agree closely" else if (mean(agree$rank_r) >= 0.65) "mostly agree" else "often disagree",
             sprintf("lowest: %s %.2f", agree$position[which.min(agree$rank_r)], min(agree$rank_r)))
  dis <- both %>% mutate(gap = rank_par - rank_gar) %>% arrange(desc(abs(gap))) %>% head(20) %>%
    transmute(player_name, team, position, minutes_played, rank_gar, rank_par, of = n,
              rated_higher_by = if_else(gap > 0, "GAR", "PAR"))
  write_csv(dis, file.path(OUT_DIR, "biggest_disagreements.csv"))
  cat("   Biggest disagreements written to biggest_disagreements.csv\n\n")
  g10 <- ggplot(agree, aes(rank_r, factor(position, levels = rev(POS_ORDER)))) +
    geom_col(fill = "#44546A", width = 0.6) +
    geom_text(aes(label = sprintf("%.2f", rank_r)), hjust = -0.2, size = 3.5) +
    scale_x_continuous(limits = c(0, 1.08)) +
    labs(title = "How similarly do GAR and PAR rank players?",
         subtitle = sprintf("Rank correlation within each position, players with %d+ minutes, %d (1 = same order)", QUAL_MIN, LATEST),
         x = NULL, y = NULL) + theme_v
  save_chart(g10, "05_gar_vs_par_agreement.png", h = 4.5)
}

# ---- Summary ----------------------------------------------------------------
res <- bind_rows(results)
write_csv(res, file.path(OUT_DIR, "validation_results.csv"), na = "")
cmp <- res %>% filter(model %in% MODELS) %>%
  select(check, measure, season, model, value, baseline, baseline_name) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  relocate(any_of(MODELS), .after = season)
write_csv(cmp, file.path(OUT_DIR, "validation_compare.csv"), na = "")

# Seasons side by side for the checks that run every season
by_season <- res %>% filter(model %in% MODELS, !is.na(value), check %in% c("1. Non-conference games", "2. Massey ratings",
                                                                          "3. Coaches' choices", "4. Reliability", "5. Team-strength leak"),
                            !grepl("extra R-squared", measure))
if (n_distinct(by_season$season) > 1) {
  bs <- bind_rows(by_season %>% transmute(check, season, who = model, value),
                  by_season %>% distinct(check, season, baseline) %>% filter(!is.na(baseline)) %>%
                    transmute(check, season, who = "Baseline", value = baseline))
  g8 <- ggplot(bs, aes(value, factor(season, levels = rev(unique(c(as.character(CHECK_SEASONS), POOLED)))), colour = who)) +
    geom_line(aes(group = season), colour = "grey85", linewidth = 2) +
    geom_point(size = 3.5) +
    scale_colour_manual(values = COL, name = NULL) +
    facet_wrap(~ check, ncol = 1, scales = "free_x") +
    labs(title = "Do the results hold up in every season?",
         subtitle = "Each check run on each season separately, and pooled where it applies. Team-strength leak: lower is better.",
         x = NULL, y = NULL) + theme_v
  save_chart(g8, "08_by_season.png", h = 9)
}

# Scorecard chart: each model vs the baseline on the checks that have one
# (pooled seasons where available, otherwise the latest season)
sc_season <- function(ck) { s <- unique(res$season[res$check == ck]); if (!is.na(POOLED) && POOLED %in% s) POOLED else as.character(LATEST) }
sc <- res %>% filter(model %in% MODELS, !is.na(value), !is.na(baseline)) %>%
  filter(check %in% c("1. Non-conference games", "2. Massey ratings", "3. Coaches' choices",
                      "7. Year to year", "8. Awards", "11. Next-season forecast"),
         !grepl("extra R-squared|stayed at the same team", measure)) %>%
  group_by(check) %>% filter(season == sc_season(first(check))) %>% ungroup() %>%
  mutate(row = if_else(check %in% c("7. Year to year", "11. Next-season forecast"), paste0(check, ": ", sub("^r (with|between) ", "", measure)), check),
         row = sub(", players who", ",", row))
if (nrow(sc)) {
  scl <- bind_rows(sc %>% transmute(row, who = model, value),
                   sc %>% distinct(row, baseline) %>% transmute(row, who = "Baseline", value = baseline))
  g0 <- ggplot(scl, aes(value, factor(row, levels = rev(unique(row))), colour = who)) +
    geom_line(aes(group = row), colour = "grey85", linewidth = 2) +
    geom_point(size = 4) +
    scale_colour_manual(values = COL, name = NULL) +
    labs(title = sprintf("Validation scorecard: %s vs simple baselines", paste(MODELS, collapse = " and ")),
         subtitle = sprintf("Higher is better on every row. Grey = the best simple stat or raw measure.%s",
                            if (!is.na(POOLED)) sprintf(" Team checks: %s.", POOLED) else ""),
         x = NULL, y = NULL) + theme_v
  save_chart(g0, "00_scorecard.png", w = 9, h = 2 + 0.42 * n_distinct(scl$row))
}

cat("==================== SUMMARY ====================\n")
for (ck in unique(res$check)) {
  cat(ck, "\n")
  for (ms in unique(res$measure[res$check == ck])) {
    for (sn in unique(res$season[res$check == ck & res$measure == ms])) {
      r <- res %>% filter(check == ck, measure == ms, season %in% sn)
      vals <- paste(sprintf("%s %s", r$model, ifelse(is.na(r$value), "—", sprintf("%.2f", r$value))), collapse = " | ")
      b <- r$baseline[1]
      cat(sprintf("   %s%s: %s%s\n", ms, if (!is.na(sn)) sprintf(" [%s]", sn) else "", vals,
                  if (!is.na(b)) sprintf(" | baseline %.2f (%s)", b, r$baseline_name[1]) else ""))
      for (i in seq_len(nrow(r))) if (!is.na(r$verdict[i]))
        cat(sprintf("      %s -> %s\n", r$model[i], r$verdict[i]))
    }
  }
}
cat(sprintf("\nCharts and CSVs are in %s/\n", OUT_DIR))
