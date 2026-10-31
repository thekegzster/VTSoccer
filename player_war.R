# =============================================================================
# Player WAR (wins above replacement) for NCAA D1 men's soccer
#
# Inputs (in DATA_DIR):
#   simulated_player_metrics.csv   one row per player per season (Wyscout-style)
#   simulated_standings.csv        team records + strength of schedule (set STANDINGS_FILE;
#                                  use rpi_standings_all_fixed.csv for the real records)
#   war_weights.csv                which stats count for each position, and
#                                  whether more of the stat is good (+1), bad (-1),
#                                  or excluded (0)
#
# Outputs (in OUT_DIR): player_war.csv, team_war.csv, weights_used.csv, charts
#
# How it works:
#   1. Build each position's stats (per 90) from the raw columns.
#   2. Shrink small-sample players toward the position average.
#   3. Adjust each stat for strength of schedule.
#   4. Scale each stat 0-1 within position and season (5th-95th percentile)
#   5. Weight the stats. "learned" mode fits the weights to team winning %:
#      you choose each stat's direction in war_weights.csv, the data chooses
#      its size. "manual" mode uses manual_weight from the CSV instead.
#   6. Index = sum of weight x scaled stat. Convert to wins, compare with a
#      replacement-level player at the same position, multiply by minutes.
#
# Because the weights are fit to winning, the index is already measured in
# wins, so: WAR = (player index - replacement index) x minutes / 90.
# =============================================================================

# install.packages(c("readr", "dplyr", "tidyr", "purrr", "ggplot2", "glmnet", "scales"))
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(glmnet)
library(scales)

# ---- Settings ---------------------------------------------------------------

DATA_DIR     <- "."            # folder with the input CSVs
OUT_DIR      <- "war_output"
STANDINGS_FILE <- "simulated_standings.csv"   # simulated results (players cause winning).
                               # Use "rpi_standings_all_fixed.csv" with real player data.
WEIGHTS_FILE <- "war_weights_compiled.csv"  # which stat list to use:
                               #   "war_weights_original.csv" = original (goals, assists, duplicates)
                               #   "war_weights_compiled.csv" = one stat per idea, xG and xA
WEIGHT_MODE  <- "learned"      # "learned" or "manual"
MIN_QUAL     <- 300            # minutes to count toward min/max and replacement level
STAT_PRIOR_MIN <- c(prevented_goals = 1800)  # stats that need more minutes before they're
                               # trusted: goalkeeper shot-stopping swings a lot by luck, so a keeper
                               # is treated as if he also had 1,800 average minutes (PRIOR_MIN for others)
PRIOR_MIN    <- 270            # shrinkage: a player is treated as if he also played
                               # this many minutes at the position average
SCALE_LO     <- 0.05           # percentile used as the scaling "min"
SCALE_HI     <- 0.95           # percentile used as the scaling "max"
REPL_PCTL    <- 0.20           # replacement level = this percentile of qualified
                               # players at the position, each season
ADJUST_SOS   <- TRUE           # adjust stats for strength of schedule
FIT_EXCLUDE  <- c(2020)        # seasons left out of the weight fit (COVID)
FOCUS_TEAM   <- "Virginia Tech"
LATEST       <- 2025           # most recent complete season (for charts); NA = latest in the data
VERSION      <- ""             # label for this run in the scorecard. Leave "" to name it
                               # automatically from the weights file and mode, e.g.
                               # "war_weights_compiled (learned)". Set your own label when you
                               # change something else (shrinkage, replacement level, ...)
SCORE_MIN    <- 600            # minutes needed to count in the scorecard

# ---- Which data: simulated or real ------------------------------------------
# "simulated" = simulated_player_metrics.csv / simulated_standings.csv (many seasons)
# "real"      = the files prepare_real_data.R writes from the Wyscout export
#               (one season, conference games only). Run prepare_real_data.R first.
DATA_SOURCE  <- "real"
PLAYERS_FILE <- "simulated_player_metrics.csv"
if (DATA_SOURCE == "real") {
  PLAYERS_FILE   <- "real_player_metrics.csv"
  STANDINGS_FILE <- "real_standings.csv"
  WEIGHTS_FILE   <- "war_weights_real.csv"   # stat list without stats the export lacks
  OUT_DIR        <- "war_output_real"
  ADJUST_SOS     <- FALSE   # the Adjusted sheet already corrects for conference strength
  LATEST         <- NA      # = the latest season in the file (2026)
  MIN_QUAL       <- 270     # a season so far is ~5-11 conference games, so lower minute cut-offs
  SCORE_MIN      <- 360
}

# Optional overrides from the command line, so you can switch without editing:
#   Rscript player_war.R war_weights_original.csv manual
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) WEIGHTS_FILE <- args[1]
if (length(args) >= 2) WEIGHT_MODE  <- args[2]
if (length(args) >= 3) VERSION      <- args[3]

stopifnot(WEIGHT_MODE %in% c("learned", "manual"))
if (!file.exists(file.path(DATA_DIR, WEIGHTS_FILE))) stop("Weights file not found: ", WEIGHTS_FILE)
if (VERSION == "") VERSION <- sprintf("%s (%s)", sub("\\.csv$", "", WEIGHTS_FILE), WEIGHT_MODE)
cat(sprintf("Weights: %s | mode: %s | scorecard label: %s\n", WEIGHTS_FILE, WEIGHT_MODE, VERSION))

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- 1. Load ----------------------------------------------------------------

players <- read_csv(file.path(DATA_DIR, PLAYERS_FILE),
                    show_col_types = FALSE) %>%
  filter(minutes_played > 0)

if (is.na(LATEST)) LATEST <- max(players$season_year)
n_seasons <- n_distinct(players$season_year)
DATA_CAPTION <- if (DATA_SOURCE == "real") sprintf("Wyscout data, conference games, %d season to date", LATEST) else
  "Simulated player data"
span_yrs <- intersect(sort(unique(players$season_year)), setdiff((LATEST - 4):LATEST, FIT_EXCLUDE))
SEASON_SPAN <- if (length(span_yrs) == 1) as.character(span_yrs) else
  sprintf("%d-%d%s", min(span_yrs), max(span_yrs), if (any(FIT_EXCLUDE %in% min(span_yrs):max(span_yrs))) " (2020 excluded)" else "")
cat(sprintf("Data: %s (%d players, %d season%s; charts show %d)\n", PLAYERS_FILE, nrow(players),
            n_seasons, if (n_seasons == 1) "" else "s", LATEST))

standings <- read_csv(file.path(DATA_DIR, STANDINGS_FILE),
                      show_col_types = FALSE) %>%
  select(season_year, team, conference, rpi_rank, sos_rank,
         overall_w, overall_l, overall_t, any_of(c("games", "win_pct"))) %>%
  distinct(season_year, team, .keep_all = TRUE)

weights_cfg <- read_csv(file.path(DATA_DIR, WEIGHTS_FILE),
                        show_col_types = FALSE) %>%
  filter(direction != 0)

# games and win_pct can be given directly (real_standings.csv does: the export covers
# conference games only, so games come from team minutes); otherwise from the record
for (nm in c("games", "win_pct")) if (!nm %in% names(standings)) standings[[nm]] <- NA_real_

# ---- 2. Team context: record and schedule strength --------------------------

teams <- standings %>%
  mutate(games   = coalesce(games, overall_w + overall_l + overall_t),
         win_pct = coalesce(win_pct, (overall_w + 0.5 * overall_t) / (overall_w + overall_l + overall_t))) %>%
  group_by(season_year) %>%
  # 1 = toughest schedule that season, 0 = easiest
  mutate(sos_pct = 1 - percent_rank(sos_rank)) %>%
  group_by(conference) %>%
  # rows without SOS (hand-entered 2018 ACC) get their conference's usual value
  mutate(sos_pct = coalesce(sos_pct, median(sos_pct, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(sos_pct = coalesce(sos_pct, 0.5)) %>%
  select(season_year, team, rpi_rank, games, win_pct, sos_pct)

players <- players %>%
  inner_join(teams, by = c("season_year", "team"))

# ---- 3. Build the stats in war_weights.csv ----------------------------------

z <- function(x) coalesce(x, 0)   # missing count/rate -> 0

# Teams that rarely have the ball make more defensive actions just because
# they're defending more. Scale defensive volume to a 50% possession game.
padj <- function(x, poss) x * (100 - 50) / (100 - poss)

# The on-ball version: teams that have the ball more make more on-ball mistakes
# (more touches, more chances to lose it). Scale to a 50% possession game.
padj_own <- function(x, poss) x * 50 / poss

# Columns a data file doesn't have (e.g. pressing duels in the real export) are added
# as empty, so every stat below can be defined; stats that end up empty are dropped
# from the weights further down.
stat_cols <- c(
  "Accelerations / 90",
  "Aerial duels / 90 minutes",
  "Aerial duels won (%)",
  "Assists / 90",
  "Clearances / 90",
  "Crosses / 90",
  "Defensive duels / 90 minutes",
  "Defensive duels won (%)",
  "Dribbles %",
  "Dribbles / 90",
  "Duels won (%)",
  "Exits, per 90 mins",
  "Goals / 90",
  "Interceptions / per 30 minutes of opponent possession",
  "Key passes / 90",
  "Loose ball duels / 90 minutes",
  "Losses / 90",
  "Offensive Duels %",
  "Offensive Duels / 90",
  "Pass accuracy",
  "Passes / 90",
  "Pressing duels, per 90",
  "Prevented goals / 90",
  "Red cards / 90 minutes",
  "Save rate (%)",
  "Shot assists / 90",
  "Shots / 90",
  "Shots against / 90",
  "Shots blocked / 90",
  "Shots on target %",
  "Sliding tackles / per 30 minutes of opponent possession",
  "Through passes / 90",
  "Yellow cards / 90 minutes",
  "xA / 90",
  "xG / 90")
for (nm in setdiff(stat_cols, names(players))) players[[nm]] <- NA_real_
if (!"team_possession_pct" %in% names(players)) players$team_possession_pct <- 50
players$team_possession_pct <- coalesce(players$team_possession_pct, 50)

players <- players %>%
  mutate(
    accelerations    = z(`Accelerations / 90`),
    crosses          = z(`Crosses / 90`),
    def_duels_won    = padj(z(`Defensive duels / 90 minutes`) * z(`Defensive duels won (%)`) / 100,
                            team_possession_pct),
    def_duel_win_pct = `Defensive duels won (%)`,
    sliding_tackles  = z(`Sliding tackles / per 30 minutes of opponent possession`),
    key_passes       = z(`Key passes / 90`),
    clearances       = padj(z(`Clearances / 90`), team_possession_pct),
    pressing         = z(`Pressing duels, per 90`),
    # possession-adjusted, so players on teams that rarely have the ball
    # don't get credit just for having more chances to intercept
    interceptions    = z(`Interceptions / per 30 minutes of opponent possession`),
    loose_ball_duels = z(`Loose ball duels / 90 minutes`),
    dribbles_won     = z(`Dribbles / 90`) * z(`Dribbles %`) / 100,
    aerial_won       = z(`Aerial duels / 90 minutes`) * z(`Aerial duels won (%)`) / 100,
    aerial_win_pct   = `Aerial duels won (%)`,
    lost_balls       = padj_own(z(`Losses / 90`), team_possession_pct),
    cards            = z(`Yellow cards / 90 minutes`) + 2 * z(`Red cards / 90 minutes`),
    pass_acc         = `Pass accuracy`,
    # Expected goals / assists: shot and chance quality, much less luck-driven
    # than actual goals and assists over a 20-game season
    xg               = z(`xG / 90`),
    xa               = z(`xA / 90`),
    # The stats below are no longer used in war_weights.csv (replaced by xg/xa
    # and the win % versions) but stay defined so you can switch back.
    goals            = z(`Goals / 90`),
    chances_created  = z(`Shot assists / 90`),
    shots            = z(`Shots / 90`),
    shots_on_target  = z(`Shots / 90`) * z(`Shots on target %`) / 100,
    assists          = z(`Assists / 90`),
    passes           = z(`Passes / 90`),
    through_passes   = z(`Through passes / 90`),
    duels_won        = z(`Offensive Duels / 90`) * z(`Offensive Duels %`) / 100 + aerial_won,
    shots_against    = `Shots against / 90`,
    exits            = `Exits, per 90 mins`,
    # In the real export only (empty on simulated data):
    shots_blocked    = padj(z(`Shots blocked / 90`), team_possession_pct),
    duel_win_pct     = `Duels won (%)`,
    prevented_goals  = `Prevented goals / 90`,   # GK: xG faced minus goals conceded, per 90
    save_pct         = `Save rate (%)`
  )

missing_stats <- setdiff(unique(weights_cfg$stat), names(players))
empty_stats <- Filter(function(st) st %in% names(players) && all(is.na(players[[st]]) | players[[st]] == 0),
                      unique(weights_cfg$stat))
if (length(empty_stats)) {
  cat("Not in this data, dropped from the weights:", paste(empty_stats, collapse = ", "), "\n")
  weights_cfg <- filter(weights_cfg, !stat %in% empty_stats)
}
if (length(missing_stats)) stop(WEIGHTS_FILE, " uses stats that aren't defined: ",
                                paste(missing_stats, collapse = ", "))

id_cols <- c("season_year", "conference", "team", "player_id", "player_name",
             "position", "class_year", "minutes_played", "games", "win_pct",
             "sos_pct", "rpi_rank")

long <- players %>%
  select(all_of(id_cols), all_of(unique(weights_cfg$stat))) %>%
  pivot_longer(all_of(unique(weights_cfg$stat)), names_to = "stat", values_to = "raw") %>%
  semi_join(weights_cfg, by = c("position", "stat")) %>%
  mutate(qualified = minutes_played >= MIN_QUAL)

# ---- 4. Fill gaps and shrink small samples ----------------------------------

long <- long %>%
  group_by(season_year, position, stat) %>%
  mutate(prior = mean(raw[qualified], na.rm = TRUE),
         raw   = coalesce(raw, prior),
         pm    = coalesce(unname(STAT_PRIOR_MIN[stat]), PRIOR_MIN),
         value = (raw * minutes_played + prior * pm) / (minutes_played + pm)) %>%
  ungroup()

# ---- 5. Strength-of-schedule adjustment -------------------------------------
# For each position x stat, estimate how much a tougher schedule changes the
# stat for players on teams with the same record, then remove that effect

if (ADJUST_SOS) {
  sos_fx <- long %>%
    filter(qualified) %>%
    group_by(position, stat) %>%
    summarise(sos_slope = coef(lm(value ~ sos_pct + win_pct))[["sos_pct"]],
              .groups = "drop")
  long <- long %>%
    left_join(sos_fx, by = c("position", "stat")) %>%
    mutate(value = value - sos_slope * (sos_pct - 0.5))
}

# ---- 6. Scale each stat 0-1 within position and season ----------------------

long <- long %>%
  group_by(season_year, position, stat) %>%
  mutate(lo = quantile(value[qualified], SCALE_LO, na.rm = TRUE),
         hi = quantile(value[qualified], SCALE_HI, na.rm = TRUE),
         scaled = if_else(hi > lo, pmin(pmax((value - lo) / (hi - lo), 0), 1), 0.5),
         # league average for the position, weighted by minutes
         scaled_avg = weighted.mean(scaled, minutes_played)) %>%
  ungroup() %>%
  mutate(feature = paste(position, stat, sep = ":"))

# ---- 7. Weights -------------------------------------------------------------
# Team-level feature: how far above/below average the team's players at a
# position were on a stat, weighted by share of the team's minutes.
#   X = sum over players of (scaled - avg) x minutes / (90 x games)
# Team win % = sum of weight x X + schedule effect. A player's share of team
# wins is then weight x (scaled - avg) x minutes / 90.

team_features <- long %>%
  mutate(x = (scaled - scaled_avg) * minutes_played / (90 * games)) %>%
  group_by(season_year, team, feature) %>%
  summarise(x = sum(x), .groups = "drop") %>%
  pivot_wider(names_from = feature, values_from = x, values_fill = 0) %>%
  inner_join(teams, by = c("season_year", "team"))

feat_names <- unique(long$feature)
cfg <- weights_cfg %>%
  mutate(feature = paste(position, stat, sep = ":")) %>%
  filter(feature %in% feat_names)

fit_rows <- team_features %>% filter(!season_year %in% FIT_EXCLUDE)

if (WEIGHT_MODE == "learned") {
  X <- as.matrix(fit_rows[, c(cfg$feature, "sos_pct")])
  y <- fit_rows$win_pct
  # direction is fixed by war_weights.csv; the fit only picks the size
  lower <- c(ifelse(cfg$direction > 0, 0, -Inf), -Inf)
  upper <- c(ifelse(cfg$direction > 0, Inf, 0),  Inf)
  # leave one season out; with one season (real data), 10 random groups of teams
  folds <- if (n_distinct(fit_rows$season_year) >= 3) as.integer(factor(fit_rows$season_year)) else
    sample(rep_len(1:10, nrow(fit_rows)))

  cv <- cv.glmnet(X, y, alpha = 0, foldid = folds, keep = TRUE,
                  lower.limits = lower, upper.limits = upper,
                  penalty.factor = c(rep(1, nrow(cfg)), 0))
  b <- coef(cv, s = "lambda.min")[, 1]
  cfg$weight <- unname(b[cfg$feature])

  pred_oos <- cv$fit.preval[, which(cv$lambda == cv$lambda.min)]
  r2_oos   <- 1 - sum((y - pred_oos)^2) / sum((y - mean(y))^2)
  r2_sos   <- summary(lm(y ~ fit_rows$sos_pct))$r.squared
  cat(sprintf("\nTeam win %% explained, held-out data: %.2f (schedule alone: %.2f)\n",
              r2_oos, r2_sos))
} else {
  cfg$weight <- cfg$direction * cfg$manual_weight
}

# Position calibration: converts a position's index into win % units.
# In learned mode the weights are already in win % so this is 1.
pos_index_feats <- long %>%
  left_join(cfg %>% select(position, stat, weight), by = c("position", "stat")) %>%
  mutate(x = weight * (scaled - scaled_avg) * minutes_played / (90 * games)) %>%
  group_by(season_year, team, position) %>%
  summarise(x = sum(x), .groups = "drop") %>%
  pivot_wider(names_from = position, values_from = x, values_fill = 0, names_prefix = "pos_") %>%
  inner_join(teams, by = c("season_year", "team"))

if (WEIGHT_MODE == "learned") {
  calib <- tibble(position = unique(cfg$position), beta = 1)
} else {
  pos_cols <- grep("^pos_", names(pos_index_feats), value = TRUE)
  m <- lm(reformulate(c(pos_cols, "sos_pct"), "win_pct"),
          data = filter(pos_index_feats, !season_year %in% FIT_EXCLUDE))
  calib <- tibble(position = sub("^pos_", "", pos_cols), beta = coef(m)[pos_cols])
  if (any(calib$beta <= 0)) warning("Some positions got a negative calibration; ",
                                    "check manual weights: ",
                                    paste(calib$position[calib$beta <= 0], collapse = ", "))
  cat(sprintf("\nTeam win %% explained (manual weights, in-sample): %.2f\n",
              summary(m)$r.squared))
}

weights_used <- cfg %>%
  left_join(calib, by = "position") %>%
  mutate(win_weight = weight * beta,
         # wins added over a full season (1,800 min) by going from the
         # league's 5th-percentile player on this stat to the 95th
         wins_per_season_5_to_95 = win_weight * 1800 / 90) %>%
  select(position, stat, direction, win_weight, wins_per_season_5_to_95, note) %>%
  arrange(position, desc(abs(win_weight)))
write_csv(weights_used, file.path(OUT_DIR, "weights_used.csv"))

# ---- 8. Index, replacement level, WAR ---------------------------------------

player_index <- long %>%
  left_join(weights_used %>% select(position, stat, win_weight), by = c("position", "stat")) %>%
  group_by(across(all_of(c(id_cols, "qualified")))) %>%
  summarise(index     = sum(win_weight * scaled),
            index_avg = sum(win_weight * scaled_avg),
            .groups = "drop")

player_war <- player_index %>%
  group_by(season_year, position) %>%
  mutate(repl = quantile(index[qualified], REPL_PCTL),
         index_pctile = round(100 * percent_rank(index))) %>%
  ungroup() %>%
  mutate(waa        = (index - index_avg) * minutes_played / 90,   # wins above average
         war        = (index - repl)      * minutes_played / 90,   # wins above replacement
         war_per_90 = index - repl) %>%
  group_by(season_year) %>%
  mutate(war_rank = min_rank(desc(war))) %>%
  group_by(season_year, position) %>%
  mutate(war_rank_pos = min_rank(desc(war))) %>%
  ungroup() %>%
  arrange(season_year, war_rank) %>%
  select(season_year, conference, team, player_id, player_name, position, class_year,
         minutes_played, index_pctile, war, waa, war_per_90, war_rank, war_rank_pos, rpi_rank)

write_csv(player_war, file.path(OUT_DIR, "player_war.csv"))

team_war <- player_war %>%
  group_by(season_year, conference, team, rpi_rank) %>%
  summarise(team_war = sum(war), team_waa = sum(waa), .groups = "drop") %>%
  left_join(teams %>% select(season_year, team, games, win_pct, sos_pct),
            by = c("season_year", "team"))
write_csv(team_war, file.path(OUT_DIR, "team_war.csv"))

# ---- 9. Checks --------------------------------------------------------------

chk <- team_war %>% filter(!season_year %in% FIT_EXCLUDE)
cat(sprintf("Team WAR vs team win %%: r = %.2f\n", cor(chk$team_war, chk$win_pct)))

yoy <- player_war %>%
  filter(minutes_played >= MIN_QUAL) %>%
  select(player_id, season_year, position, war_per_90) %>%
  inner_join(player_war %>% filter(minutes_played >= MIN_QUAL) %>%
               transmute(player_id, season_year = season_year - 1, next_war_per_90 = war_per_90),
             by = c("player_id", "season_year"))
yoy_r <- cor(yoy$war_per_90, yoy$next_war_per_90)
cat(sprintf("Year-to-year WAR/90 for returning players: r = %.2f (n = %d)\n", yoy_r, nrow(yoy)))

cat(sprintf("\nTop 10, %d:\n", LATEST))
print(player_war %>% filter(season_year == LATEST) %>% slice_head(n = 10) %>%
        select(player_name, team, position, minutes_played, war), n = 10)

# ---- 10. Charts -------------------------------------------------------------

pos_group <- c(GK = "Goalkeeper", CB = "Defender", FB = "Defender",
               DM = "Midfielder", CM = "Midfielder", AM = "Midfielder",
               W = "Attacker", CF = "Attacker")
group_cols <- c(Goalkeeper = "#2a78d6", Defender = "#eb6834",
                Midfielder = "#1baf7a", Attacker = "#eda100")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

theme_war <- theme_minimal(base_size = 12) +
  theme(plot.background  = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
        plot.title       = element_text(face = "bold", colour = ink),
        plot.subtitle    = element_text(colour = ink2),
        plot.caption     = element_text(colour = ink2, size = 9),
        axis.text        = element_text(colour = ink2),
        axis.title       = element_text(colour = ink2),
        legend.position  = "top", legend.justification = "left",
        legend.title     = element_blank())

save <- function(p, name, w = 9, h = 6) ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 150)

# 01: top 25 players, latest season
top <- player_war %>% filter(season_year == LATEST) %>% slice_head(n = 25) %>%
  mutate(label = paste0(player_name, " (", team, ", ", position, ")"),
         group = factor(pos_group[position], levels = names(group_cols)))
p1 <- ggplot(top, aes(war, reorder(label, war), fill = group)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = group_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = sprintf("Top 25 players by WAR, %d", LATEST),
       subtitle = "Wins added compared with a replacement-level player at the same position",
       x = "WAR", y = NULL, caption = DATA_CAPTION) +
  theme_war + theme(panel.grid.major.y = element_blank())
save(p1, "01_top25_war.png", h = 8)

# 02: focus team vs Champion (RPI 1-10) and Elite (RPI 11-25) teams, by position
by_pos <- player_war %>%
  mutate(group = case_when(team == FOCUS_TEAM ~ FOCUS_TEAM,
                           rpi_rank <= 10 ~ "Champion tier (RPI 1-10)",
                           rpi_rank <= 25 ~ "Elite tier (RPI 11-25)")) %>%
  filter(!is.na(group), !season_year %in% FIT_EXCLUDE, season_year >= LATEST - 4) %>%
  group_by(season_year, team, group, position) %>%
  summarise(war = sum(war), .groups = "drop") %>%
  complete(nesting(season_year, team, group), position, fill = list(war = 0)) %>%
  group_by(group, position) %>%
  summarise(war = mean(war), .groups = "drop") %>%
  mutate(position = factor(position, levels = c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")),
         group = factor(group, levels = c(FOCUS_TEAM, "Champion tier (RPI 1-10)", "Elite tier (RPI 11-25)")))
p2 <- ggplot(by_pos, aes(position, war, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.4) +
  scale_fill_manual(values = c("#2a78d6", "#eb6834", "#1baf7a")) +
  labs(title = sprintf("%s WAR by position vs. top programs", FOCUS_TEAM),
       subtitle = sprintf("Average total WAR per team-season, %s", SEASON_SPAN),
       x = NULL, y = "WAR", caption = DATA_CAPTION) +
  theme_war + theme(panel.grid.major.x = element_blank())
save(p2, "02_team_vs_top_programs_by_position.png")

# 03: team WAR vs actual winning %
p3 <- ggplot(chk, aes(team_war, win_pct)) +
  geom_point(colour = "#2a78d6", alpha = 0.35, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = ink2, linewidth = 0.6, formula = y ~ x) +
  geom_point(data = filter(chk, team == FOCUS_TEAM), colour = "#eb6834", size = 3) +
  scale_y_continuous(labels = percent) +
  labs(title = "Does team WAR line up with winning?",
       subtitle = sprintf("Each dot is a team-season; orange = %s. r = %.2f",
                          FOCUS_TEAM, cor(chk$team_war, chk$win_pct)),
       x = "Total team WAR", y = "Win % (ties = half)", caption = DATA_CAPTION) +
  theme_war
save(p3, "03_team_war_vs_win_pct.png")

# 04: does a player's WAR/90 carry over to next season?
p4 <- ggplot(yoy, aes(war_per_90 * 1000 / 90, next_war_per_90 * 1000 / 90)) +
  geom_point(colour = "#2a78d6", alpha = 0.15, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, colour = ink2, linewidth = 0.6, formula = y ~ x) +
  labs(title = "Is WAR a repeatable skill?",
       subtitle = sprintf("Returning players with %d+ minutes in both seasons. r = %.2f",
                          MIN_QUAL, yoy_r),
       x = "WAR per 1,000 minutes, this season", y = "WAR per 1,000 minutes, next season",
       caption = DATA_CAPTION) +
  theme_war
save(p4, "04_war_year_to_year.png")

# 05: team WAR vs RPI rank (rank 1 at the top, so better teams sit higher, like chart 03)
# Spearman because RPI is a rank: it only asks whether higher WAR goes with a better rank
rpi_r <- cor(chk$team_war, chk$rpi_rank, method = "spearman")
p5 <- ggplot(chk, aes(team_war, rpi_rank)) +
  geom_point(colour = "#2a78d6", alpha = 0.35, size = 2) +
  # a curve, not a straight line: ranks stop at 1, so the top flattens out
  geom_smooth(method = "loess", se = FALSE, colour = ink2, linewidth = 0.6, formula = y ~ x) +
  geom_point(data = filter(chk, team == FOCUS_TEAM), colour = "#eb6834", size = 3) +
  scale_y_reverse(breaks = c(1, 50, 100, 150, 200)) +
  labs(title = "Does team WAR line up with RPI?",
       subtitle = sprintf("Each dot is a team-season; orange = %s. Spearman r = %.2f",
                          FOCUS_TEAM, rpi_r),
       x = "Total team WAR", y = "RPI rank", caption = DATA_CAPTION) +
  theme_war
save(p5, "05_team_war_vs_rpi.png")
cat(sprintf("Team WAR vs RPI rank: Spearman r = %.2f\n", rpi_r))

# ---- 11. Scorecard ----------------------------------------------------------
# Grades the method on players, not teams. Needs the simulated true_talent
# column (the answer key), so it is skipped automatically on real data.
#
# For each position (players with SCORE_MIN+ minutes, FIT_EXCLUDE seasons left out):
#   r_talent         WAR/90 vs true talent. Higher = better.
#   r_talent_no_team same, after removing team strength from both. This is the
#                    main score: does WAR find good players, not good teams?
#   r_team           WAR/90 vs team strength. Some is expected (good programs
#                    recruit better players), so compare it with talent_vs_team.
#   talent_vs_team   true talent vs team strength: the "honest" amount of team
#                    link. r_team well above this = WAR is crediting the team.
# Uses WAR per 90, not total WAR, so playing time doesn't count as skill.
# Each run is added to scorecard_history.csv under VERSION (a rerun with the
# same VERSION replaces its old rows).

pos_order <- c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")

if (all(c("true_talent", "team_strength_z") %in% names(players))) {

  sc_data <- player_war %>%
    filter(minutes_played >= SCORE_MIN, !season_year %in% FIT_EXCLUDE) %>%
    inner_join(players %>% select(player_id, season_year, true_talent, team_strength_z),
               by = c("player_id", "season_year"))

  # correlation of x and y after removing z from both (partial correlation)
  partial_r <- function(x, y, z) cor(resid(lm(x ~ z)), resid(lm(y ~ z)))

  by_position <- sc_data %>%
    group_by(position) %>%
    summarise(n                = n(),
              r_talent         = cor(war_per_90, true_talent),
              r_talent_no_team = partial_r(war_per_90, true_talent, team_strength_z),
              r_team           = cor(war_per_90, team_strength_z),
              talent_vs_team   = cor(true_talent, team_strength_z),
              .groups = "drop")

  scorecard <- bind_rows(
    by_position,
    by_position %>%   # simple average across positions, so each position counts equally
      summarise(position = "Average", n = sum(n),
                across(c(r_talent, r_talent_no_team, r_team, talent_vs_team), mean))
  ) %>%
    pivot_longer(-c(position, n), names_to = "metric", values_to = "value") %>%
    bind_rows(tibble(
      position = "Model",
      n        = NA_integer_,
      metric   = c("heldout_r2_win_pct", "year_to_year_r", "team_r_win_pct", "team_spearman_rpi"),
      value    = c(if (WEIGHT_MODE == "learned") r2_oos else NA_real_,
                   yoy_r, cor(chk$team_war, chk$win_pct), rpi_r))) %>%
    mutate(version = VERSION, weight_mode = WEIGHT_MODE,
           run_time = format(Sys.time(), "%Y-%m-%d %H:%M"), value = round(value, 3)) %>%
    relocate(version, weight_mode, run_time)

  hist_file <- file.path(OUT_DIR, "scorecard_history.csv")
  history <- if (file.exists(hist_file)) {
    read_csv(hist_file, col_types = cols(n = col_integer(), value = col_double(),
                                         .default = col_character())) %>%
      filter(version != VERSION)
  } else NULL
  history <- bind_rows(history, scorecard)
  write_csv(scorecard, file.path(OUT_DIR, "scorecard.csv"))
  write_csv(history, hist_file)

  cat(sprintf("\nScorecard (%s): r with true talent, team strength removed\n", VERSION))
  print(scorecard %>% filter(metric == "r_talent_no_team") %>%
          select(position, n, value) %>%
          arrange(match(position, c(pos_order, "Average"))), n = 20)

  # 06: this run, by position
  metric_labels <- c(r_talent_no_team = "WAR vs true talent, team strength removed (main score)",
                     r_talent         = "WAR vs true talent",
                     r_team           = "WAR vs team strength",
                     talent_vs_team   = "True talent vs team strength (expected team link)")
  metric_cols   <- c("#1baf7a", "#2a78d6", "#eb6834", "#a8a6a0")
  sc_plot <- scorecard %>%
    filter(metric %in% names(metric_labels)) %>%
    mutate(position = factor(position, levels = rev(c(pos_order, "Average"))),
           metric   = factor(metric_labels[metric], levels = metric_labels))
  model_vals <- with(filter(scorecard, position == "Model"), setNames(value, metric))

  p6 <- ggplot(sc_plot, aes(value, position, colour = metric, shape = metric,
                            group = factor(metric, levels = rev(metric_labels)))) +  # main score on top
    geom_vline(xintercept = 0, colour = ink2, linewidth = 0.3) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = metric_cols) +
    scale_shape_manual(values = c(16, 16, 17, 4)) +
    scale_x_continuous(limits = c(min(0, sc_plot$value), 1), breaks = seq(0, 1, 0.2)) +
    guides(colour = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) +
    labs(title = sprintf("WAR scorecard: %s", VERSION),
         subtitle = sprintf(paste0("Correlations for players with %d+ minutes\nModel checks: ",
                                   "held-out R-squared %s, year-to-year r %.2f, team win %% r %.2f"),
                            SCORE_MIN,
                            ifelse(is.na(model_vals[["heldout_r2_win_pct"]]), "n/a",
                                   sprintf("%.2f", model_vals[["heldout_r2_win_pct"]])),
                            model_vals[["year_to_year_r"]], model_vals[["team_r_win_pct"]]),
         x = "Correlation", y = NULL, caption = "Simulated player data; true talent is the simulation's answer key") +
    theme_war + theme(panel.grid.major.y = element_blank())
  save(p6, "06_scorecard.png", h = 7)

  # 07: main score across versions (only once there are at least two)
  if (n_distinct(history$version) >= 2) {
    hist_plot <- history %>%
      filter(metric == "r_talent_no_team") %>%
      mutate(position = factor(position, levels = c(pos_order, "Average")),
             # versions are shown in alphabetical order of their labels
             version  = factor(version, levels = sort(unique(history$version))))
    p7 <- ggplot(hist_plot, aes(position, value, colour = version, group = version)) +
      geom_line(linewidth = 0.6, alpha = 0.6) +
      geom_point(size = 2.8) +
      geom_hline(yintercept = 0, colour = ink2, linewidth = 0.3) +
      # same palette as the other charts; ggplot's defaults take over past 6 versions
      { if (n_distinct(hist_plot$version) <= 6)
          scale_colour_manual(values = c("#a8a6a0", "#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#7b5cd6")) } +
      scale_y_continuous(limits = c(min(0, hist_plot$value), 1)) +
      labs(title = "Is the method getting better?",
           subtitle = "WAR vs true talent with team strength removed, by version (higher is better)",
           x = NULL, y = "Correlation", caption = DATA_CAPTION) +
      theme_war + theme(panel.grid.major.x = element_blank())
    save(p7, "07_scorecard_history.png")
  }
} else {
  cat("\nNo true_talent column: scorecard skipped (expected on real data).\n")
}

cat("\nDone. Outputs in", OUT_DIR, "\n")
