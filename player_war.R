# =============================================================================
# Player WAR (wins above replacement) for NCAA D1 men's soccer
#
# Inputs (in DATA_DIR; run prepare_real_data.R first):
#   real_player_metrics.csv   one row per player per season (Wyscout, conference games)
#   real_standings.csv        team records and Massey ratings
#   par_categories.csv        THE FILE TO EDIT: which stats count at each position,
#                             grouped into 3-4 categories, each stat's direction
#                             (+1 more is better, -1 fewer is better, 0 left out) and
#                             weight inside its category, and optionally a fixed
#                             value for a category instead of a learned one
#
# Outputs (in OUT_DIR): player_par.csv, team_par.csv, par_weights.csv (every
#   category's and stat's value in points per season: the file to READ),
#   par_breakdown.csv (for the app), charts. With OUTCOME "wins": the same named war.
#
# How it works:
#   1. Build each position's stats (per 90) from the raw columns.
#   2. Shrink small-sample players toward the position average.
#   3. Scale each stat within position and season: 0 = 5th percentile, 1 = 95th
#      (flipped for stats where fewer is better).
#   4. Combine the stats into categories (e.g. a CB's defending, aerial, passing and
#      goal threat): a weighted average of the scaled stats.
#   5. Learn one weight per category: how much a team's Massey rating rises when its
#      players at that position are above average in the category (TARGET), turned
#      into league points per game. Fewer weights than one per stat = steadier weights.
#   6. Index = sum of weight x category score. Compare with a replacement-level player
#      at the same position, multiply by minutes / 90.
#   7. An 80% range for every rating, from each stat's sampling noise (as in GAR).
# =============================================================================

# install.packages(c("readr", "dplyr", "tidyr", "purrr", "ggplot2", "glmnet", "scales"))
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(glmnet)
library(scales)
source("model_helpers.R")   # shrink_stats(), team_folds()

# ---- Settings ---------------------------------------------------------------

DATA_DIR     <- "."            # folder with the input CSVs (written by prepare_real_data.R)
PLAYERS_FILE <- "real_player_metrics.csv"
STANDINGS_FILE <- "real_standings.csv"
OUT_DIR      <- "war_output_real"   # "par_output_real" when OUTCOME is "points"
WEIGHTS_FILE <- "par_categories.csv"   # stats and categories at each position (edit this)
                               # (a file without a category column, like the old
                               # war_weights_real.csv, gives every stat its own category)
WEIGHT_MODE  <- "learned"      # "learned": categories without a fixed_points_per_season
                               #   are learned from the data, the rest are fixed
                               # "manual": every category must have fixed_points_per_season
TARGET       <- "massey"       # what the category weights are fitted to:
                               #   "massey" = each team's Massey rating (uses every game and
                               #     the opponents, so it's steadier than a 5-12 game record),
                               #     converted to points per game afterwards
                               #   "outcome" = the team's conference points per game (or win %)
RANGE_LEVEL  <- 0.80           # uncertainty range shown for each player (as in GAR)
MIN_QUAL     <- 270            # minutes to count toward min/max and replacement level
                               # (a season is ~5-12 conference games)
SHRINK_MODE  <- "per_stat"     # small-sample blending toward the position average:
                               #   "per_stat" = each stat as much as its own noise calls for
                               #     (xG and xA much more than pass accuracy; win-% stats by
                               #     attempts); see shrink_stats() in model_helpers.R
                               #   "minutes"  = the old way: PRIOR_MIN minutes for every stat
PRIOR_MIN    <- 270            # "minutes" mode: a player is treated as if he also played
                               # this many minutes at the position average
STAT_PRIOR_MIN <- c(prevented_goals = 1800)  # "minutes" mode only: more for noisier stats
MAKE_PLOTS   <- TRUE           # FALSE = skip the charts and only write the CSV files (much faster)
NON_PENALTY  <- TRUE           # use non-penalty xG (npxG) when the data has it
SCALE_LO     <- 0.05           # percentile used as the scaling "min"
SCALE_HI     <- 0.95           # percentile used as the scaling "max"
CAP_SCALE    <- FALSE          # FALSE = no cap: a player beyond the 95th percentile keeps
                               # counting (scaled above 1; below the 5th percentile goes below 0).
                               # TRUE = old behavior: everything is squeezed into 0-1
REPL_PCTL    <- 0.20           # replacement level = this percentile of qualified
                               # players at the position, each season
ADJUST_SOS   <- FALSE          # adjust stats for strength of schedule; off because the
                               # Adjusted sheet already corrects for conference strength
FIT_EXCLUDE  <- c(2020)        # seasons left out of the weight fit (COVID)
FOCUS_TEAM   <- "Virginia Tech"
LATEST       <- NA             # season shown in the charts; NA = latest in the data

# ---- What to measure: points or wins ------------------------------------------
# "points" = PAR, points above replacement: the team outcome is league points per
#            game (3 for a win, 1 for a draw), which fits soccer better than wins
# "wins"   = WAR, wins above replacement: win % with a draw counted as half a win
# Everything else (stats, weights, replacement level) works the same way.
OUTCOME <- "points"

# Optional overrides from the command line, so you can switch without editing:
#   Rscript player_war.R par_categories.csv manual
args <- commandArgs(trailingOnly = TRUE)
if ("--no-plots" %in% args) { MAKE_PLOTS <- FALSE; args <- setdiff(args, "--no-plots") }
if (length(args) >= 1) WEIGHTS_FILE <- args[1]
if (length(args) >= 2) WEIGHT_MODE  <- args[2]

stopifnot(WEIGHT_MODE %in% c("learned", "manual"))
if (!file.exists(file.path(DATA_DIR, WEIGHTS_FILE))) stop("Weights file not found: ", WEIGHTS_FILE)
cat(sprintf("Weights: %s | mode: %s\n", WEIGHTS_FILE, WEIGHT_MODE))

set.seed(2026)
ABBR <- if (OUTCOME == "points") "PAR" else "WAR"
UNIT <- if (OUTCOME == "points") "points" else "wins"
if (OUTCOME == "points") OUT_DIR <- sub("war_output", "par_output", OUT_DIR)
stopifnot(OUTCOME %in% c("points", "wins"))
# Output files and columns are named for what they measure: player_par.csv with par,
# par_per_90, ... in points mode; player_war.csv with war, war_per_90, ... in wins mode
out_file <- function(nm) if (OUTCOME == "points") gsub("win_pct", "points", gsub("war", "par", nm)) else nm
out_cols <- function(df) {
  if (OUTCOME != "points") return(df)
  rename_with(df, function(x) {
    x <- sub("^win_pct$", "points_per_game", x)
    x <- sub("win_weight", "points_weight", x); x <- sub("wins_per", "points_per", x)
    x <- gsub("waa", "paa", x); gsub("war", "par", x)
  })
}
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- 1. Load ----------------------------------------------------------------

players <- read_csv(file.path(DATA_DIR, PLAYERS_FILE),
                    show_col_types = FALSE) %>%
  filter(minutes_played > 0)

if (is.na(LATEST)) LATEST <- max(players$season_year)
n_seasons <- n_distinct(players$season_year)
DATA_CAPTION <- sprintf("Wyscout data, conference games, %d season", LATEST)
span_yrs <- intersect(sort(unique(players$season_year)), setdiff((LATEST - 4):LATEST, FIT_EXCLUDE))
SEASON_SPAN <- if (length(span_yrs) == 1) as.character(span_yrs) else
  sprintf("%d-%d%s", min(span_yrs), max(span_yrs), if (any(FIT_EXCLUDE %in% min(span_yrs):max(span_yrs))) " (2020 excluded)" else "")
cat(sprintf("Data: %s (%d players, %d season%s; charts show %d)\n", PLAYERS_FILE, nrow(players),
            n_seasons, if (n_seasons == 1) "" else "s", LATEST))

standings <- read_csv(file.path(DATA_DIR, STANDINGS_FILE),
                      show_col_types = FALSE) %>%
  select(season_year, team, conference, rpi_rank, sos_rank,
         overall_w, overall_l, overall_t, any_of(c("games", "win_pct", "team_rating"))) %>%
  distinct(season_year, team, .keep_all = TRUE)
if (TARGET == "massey" && !"team_rating" %in% names(standings))
  stop(STANDINGS_FILE, " has no team_rating (Massey) column; set TARGET <- \"outcome\"")

# The categories file. A file without a category column (one weight per stat) still
# works: every stat becomes its own category.
weights_cfg <- read_csv(file.path(DATA_DIR, WEIGHTS_FILE), show_col_types = FALSE)
if (!"category" %in% names(weights_cfg)) weights_cfg$category <- weights_cfg$stat
if (!"weight_in_category" %in% names(weights_cfg)) weights_cfg$weight_in_category <- 1
if (!"fixed_points_per_season" %in% names(weights_cfg)) weights_cfg$fixed_points_per_season <- NA_real_
if (!"note" %in% names(weights_cfg)) weights_cfg$note <- NA_character_
weights_cfg <- weights_cfg %>%
  filter(direction != 0, weight_in_category > 0) %>%
  mutate(fixed_points_per_season = suppressWarnings(as.numeric(fixed_points_per_season)))
dup <- weights_cfg %>% count(position, stat) %>% filter(n > 1)
if (nrow(dup)) stop(WEIGHTS_FILE, ": a stat can be in only one category per position: ",
                    paste(dup$position, dup$stat, collapse = ", "))
stopifnot(TARGET %in% c("massey", "outcome"))

# games and win_pct can be given directly (real_standings.csv does: the export covers
# conference games only, so games come from team minutes); otherwise from the record
for (nm in c("games", "win_pct")) if (!nm %in% names(standings)) standings[[nm]] <- NA_real_

# ---- 2. Team context: record and schedule strength --------------------------

teams <- standings %>%
  mutate(games   = coalesce(games, overall_w + overall_l + overall_t),
         win_pct = coalesce(win_pct, (overall_w + 0.5 * overall_t) / (overall_w + overall_l + overall_t)),
         ppg     = (3 * overall_w + overall_t) / (overall_w + overall_l + overall_t)) %>%
  group_by(season_year) %>%
  # 1 = toughest schedule that season, 0 = easiest
  mutate(sos_pct = 1 - percent_rank(sos_rank)) %>%
  group_by(conference) %>%
  # rows without SOS (hand-entered 2018 ACC) get their conference's usual value
  mutate(sos_pct = coalesce(sos_pct, median(sos_pct, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(sos_pct = coalesce(sos_pct, 0.5)) %>%
  select(season_year, team, rpi_rank, games, win_pct, ppg, sos_pct, any_of("team_rating"))
# In points mode the model's team outcome is points per game. It is kept in the
# win_pct column so the rest of the code is unchanged.
if (OUTCOME == "points") teams <- teams %>% mutate(win_pct = coalesce(ppg, 3 * win_pct))
# What the weights are fitted to, centered within each season
teams <- teams %>%
  mutate(target = if (TARGET == "massey") team_rating else win_pct) %>%
  group_by(season_year) %>% mutate(target = target - mean(target, na.rm = TRUE)) %>% ungroup() %>%
  select(-ppg, -any_of("team_rating"))

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
  "xG / 90",
  "Accurate long passes (%)",
  "Long passes / 90 minutes",
  "Progressive passes / 90",
  "Passes to final third / 90",
  "Progressive runs / 90",
  "Passes to penalty area / 90",
  "Touches in box, per 90",
  "Duels / 90",
  "xG against / 90")
for (nm in setdiff(stat_cols, names(players))) players[[nm]] <- NA_real_
if (!"team_possession_pct" %in% names(players)) players$team_possession_pct <- 50
players$team_possession_pct <- coalesce(players$team_possession_pct, 50)

# Non-penalty xG: penalties go to whoever the coach picks to take them, so they say
# little about a player's own chances. When the data has npxG (prepare_real_data.R
# makes it), it replaces xG, and non-penalty shots replace shots, everywhere below.
use_np <- NON_PENALTY && "npxG / 90" %in% names(players) && any(!is.na(players[["npxG / 90"]]))
if (use_np) {
  players[["xG / 90"]]    <- players[["npxG / 90"]]
  players[["Shots / 90"]] <- coalesce(players[["Non-penalty shots / 90"]], players[["Shots / 90"]])
  cat("Using non-penalty xG (npxG) for xG\n")
}

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
    # Stats only some exports have (left empty when missing):
    shots_blocked    = padj(z(`Shots blocked / 90`), team_possession_pct),
    duel_win_pct     = `Duels won (%)`,
    prevented_goals  = `Prevented goals / 90`,   # GK: xG faced minus goals conceded, per 90
    save_pct         = `Save rate (%)`,
    # passing, progression and attacking stats used by the categories
    long_pass_acc    = `Accurate long passes (%)`,
    prog_passes      = z(`Progressive passes / 90`),
    final_third_passes = z(`Passes to final third / 90`),
    prog_runs        = z(`Progressive runs / 90`),
    box_passes       = z(`Passes to penalty area / 90`),
    touches_box      = z(`Touches in box, per 90`),
    off_duel_win_pct = `Offensive Duels %`
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

# A missing stat counts as the position average. Each stat is then blended toward the
# position average, as much as its own sampling noise calls for (SHRINK_MODE).
long <- long %>%
  left_join(stat_context(players), by = c("player_id", "season_year")) %>%
  shrink_stats(SHRINK_MODE, PRIOR_MIN, STAT_PRIOR_MIN)

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

# ---- 6. Scale each stat, then combine them into categories ---------------------
# Each stat is scaled within position and season: 0 = the 5th percentile, 1 = the
# 95th (no cap by default). Stats where fewer is better are flipped (1 - scaled), so
# higher is always better. A category score is the weighted average of its stats
# (weight_in_category), so it's on the same 0-1 scale.

cfg_stats <- weights_cfg %>%
  group_by(position, category) %>%
  mutate(share = weight_in_category / sum(weight_in_category)) %>%
  ungroup()

long <- long %>%
  group_by(season_year, position, stat) %>%
  mutate(lo = quantile(value[qualified], SCALE_LO, na.rm = TRUE),
         hi = quantile(value[qualified], SCALE_HI, na.rm = TRUE),
         scaled = if_else(hi > lo, (value - lo) / (hi - lo), 0.5),
         scaled = if (CAP_SCALE) pmin(pmax(scaled, 0), 1) else scaled) %>%
  ungroup() %>%
  left_join(cfg_stats %>% select(position, stat, category, direction, share), by = c("position", "stat")) %>%
  mutate(signed = if_else(direction > 0, scaled, 1 - scaled)) %>%
  group_by(season_year, position, stat) %>%
  # league average for the position, weighted by minutes
  mutate(signed_avg = weighted.mean(signed, minutes_played)) %>%
  ungroup()

cats <- long %>%
  group_by(across(all_of(c(id_cols, "qualified", "category")))) %>%
  summarise(score = sum(share * signed), .groups = "drop") %>%
  group_by(season_year, position, category) %>%
  mutate(score_avg = weighted.mean(score, minutes_played)) %>%
  ungroup() %>%
  mutate(feature = paste(position, category, sep = ":"))

# ---- 7. Category weights ------------------------------------------------------
# Team-level feature for each position x category: how far above/below average the
# team's players there were, weighted by their share of the team's minutes:
#   X = sum over players of (score - avg) x minutes / (90 x games)
# Team target (Massey rating, centered in each season) = sum of weight x X + schedule.
# A player's credit is then weight x (score - avg) x minutes / 90. Weights can't be
# negative (categories are built so that higher is better).

team_features <- cats %>%
  mutate(x = (score - score_avg) * minutes_played / (90 * games)) %>%
  group_by(season_year, team, feature) %>%
  summarise(x = sum(x), .groups = "drop") %>%
  pivot_wider(names_from = feature, values_from = x, values_fill = 0) %>%
  inner_join(teams, by = c("season_year", "team"))

cfg_cat <- cfg_stats %>%
  group_by(position, category) %>%
  summarise(fixed = if (all(is.na(fixed_points_per_season))) NA_real_ else first(na.omit(fixed_points_per_season)),
            n_fixed = n_distinct(na.omit(fixed_points_per_season)),
            stats = paste(sprintf("%s%s (%d%%)", if_else(direction < 0, "fewer ", ""), stat, round(100 * share)),
                          collapse = ", "),
            .groups = "drop") %>%
  mutate(feature = paste(position, category, sep = ":")) %>%
  filter(feature %in% unique(cats$feature))
if (any(cfg_cat$n_fixed > 1)) warning("Categories with different fixed_points_per_season on different rows ",
                                      "(the first is used): ", paste(cfg_cat$feature[cfg_cat$n_fixed > 1], collapse = ", "))
if (WEIGHT_MODE == "manual" && any(is.na(cfg_cat$fixed)))
  stop("WEIGHT_MODE manual needs fixed_points_per_season for every category; missing: ",
       paste(cfg_cat$feature[is.na(cfg_cat$fixed)], collapse = ", "))

fit_rows <- team_features %>% filter(!season_year %in% FIT_EXCLUDE, !is.na(target), !is.na(win_pct))

# Outcome units (points per game) per unit of the target. 1 when fitting to the outcome.
# Points come from games inside a conference, so a team in a strong conference earns
# fewer points for the same Massey rating: the conversion holds conference strength
# (sos_pct, the conference's rank) and the season fixed.
OUT_PER_TARGET <- if (TARGET == "massey")
  coef(lm(if (n_distinct(fit_rows$season_year) > 1) win_pct ~ target + sos_pct + factor(season_year) else win_pct ~ target + sos_pct,
          data = fit_rows))[["target"]] else 1
if (TARGET == "massey")
  cat(sprintf("Target: Massey rating. 1 Massey point = %.2f %s per game (r with %s = %.2f)\n",
              OUT_PER_TARGET, if (OUTCOME == "points") "points" else "win %", if (OUTCOME == "points") "points" else "win %",
              cor(fit_rows$target, fit_rows$win_pct)))

# fixed categories: points per season (1,800 min) -> outcome per 90 -> target units
cfg_cat$weight <- cfg_cat$fixed / (1800 / 90)
fixed_ft <- cfg_cat$feature[!is.na(cfg_cat$fixed)]
learn_ft <- cfg_cat$feature[is.na(cfg_cat$fixed)]
y <- fit_rows$target
if (length(fixed_ft))
  y <- y - as.matrix(fit_rows[, fixed_ft]) %*% (cfg_cat$weight[!is.na(cfg_cat$fixed)] / OUT_PER_TARGET)
r2_oos <- NA_real_
if (length(learn_ft)) {
  X <- as.matrix(fit_rows[, c(learn_ft, "sos_pct")])
  folds <- team_folds(fit_rows)   # a team's seasons stay in the same group
  cv <- cv.glmnet(X, y, alpha = 0, foldid = folds, keep = TRUE,
                  lower.limits = c(rep(0, length(learn_ft)), -Inf),
                  penalty.factor = c(rep(1, length(learn_ft)), 0))
  b <- coef(cv, s = "lambda.min")[, 1]
  cfg_cat$weight[is.na(cfg_cat$fixed)] <- unname(b[learn_ft]) * OUT_PER_TARGET
  pred_oos <- cv$fit.preval[, which(cv$lambda == cv$lambda.min)]
  r2_oos <- 1 - sum((y - pred_oos)^2) / sum((y - mean(y))^2)
  cat(sprintf("\nTeam %s explained, held-out data: %.2f (schedule alone: %.2f)\n",
              if (TARGET == "massey") "Massey rating" else if (OUTCOME == "points") "points per game" else "win %",
              r2_oos, summary(lm(y ~ fit_rows$sos_pct))$r.squared))
}

# par_weights.csv: what every category and stat is worth, in points per season.
#   wins_per_season_category: going from the position's 5th- to 95th-percentile
#     player on the whole category, over a full season (1,800 minutes)
#   wins_per_season_stat:     the same for one stat (its share of the category)
#   wins_per_season_per_unit: one more of the stat per 90 (or one more % point), over a
#     full season, using the latest season's 5th-95th percentile range
# (column names say "wins"; in points mode they're renamed "points")
ranges <- long %>% filter(season_year == LATEST) %>% distinct(position, stat, lo, hi)
weights_used <- cfg_stats %>%
  inner_join(cfg_cat %>% select(position, category, win_weight = weight, fixed), by = c("position", "category")) %>%
  left_join(ranges, by = c("position", "stat")) %>%
  mutate(how = if_else(is.na(fixed), "learned", "fixed"),
         wins_per_season_category = win_weight * 1800 / 90,
         wins_per_season_stat = wins_per_season_category * share,
         wins_per_season_per_unit = if_else(hi > lo, direction * wins_per_season_stat / (hi - lo), NA_real_)) %>%
  group_by(position) %>%
  mutate(cat_order = dense_rank(desc(wins_per_season_category))) %>%
  ungroup() %>%
  arrange(factor(position, levels = c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")), cat_order, desc(share)) %>%
  transmute(position, category, how, win_weight, wins_per_season_category, stat, direction,
            share_of_category = round(share, 3), wins_per_season_stat, stat_5th_pct = lo, stat_95th_pct = hi,
            wins_per_season_per_unit, note)
write_csv(out_cols(weights_used) %>% mutate(across(where(is.double), ~ as.character(signif(.x, 4)))),
          file.path(OUT_DIR, out_file("war_weights.csv")), na = "")

# ---- 8. Index, replacement level, WAR ---------------------------------------

player_index <- cats %>%
  left_join(cfg_cat %>% select(position, category, weight), by = c("position", "category")) %>%
  group_by(across(all_of(c(id_cols, "qualified")))) %>%
  summarise(index     = sum(weight * score),
            index_avg = sum(weight * score_avg),
            .groups = "drop")

# ---- 8a. Uncertainty ranges (same noise model as player_gar.R) -----------------
# One season is a limited sample. Each stat's sampling noise per 90:
#   counts per 90                       ~ the rate itself
#   percentages (duel win %, pass acc.) p(100 - p) / attempts per 90
#   xG and xA                           depend on how many shots / chances they come from
#   goals prevented (GK)                ~ xG faced
# shrinks with minutes (and the same blending as step 4), is divided by the stat's
# 5th-95th range (the scaling), and combined with the weights:
#   noise of the rating = sqrt( sum over stats of (category weight x share)^2 x noise )
# Stats are treated as independent, so ranges are a little narrow if stats move together.
z_range <- qnorm(0.5 + RANGE_LEVEL / 2)
unc <- long %>%
  left_join(cfg_cat %>% select(position, category, weight), by = c("position", "category")) %>%
  mutate(var_value  = post_var,   # noise left after the blending (shrink_stats())
         var_scaled = if_else(hi > lo, var_value / (hi - lo)^2, 0)) %>%
  group_by(player_id, season_year) %>%
  summarise(se_per_90 = sqrt(sum((weight * share)^2 * var_scaled, na.rm = TRUE)), .groups = "drop")

player_war <- player_index %>%
  group_by(season_year, position) %>%
  mutate(repl = quantile(index[qualified], REPL_PCTL),
         index_pctile = round(100 * percent_rank(index))) %>%
  ungroup() %>%
  left_join(unc, by = c("player_id", "season_year")) %>%
  mutate(waa        = (index - index_avg) * minutes_played / 90,   # wins above average
         war        = (index - repl)      * minutes_played / 90,   # wins above replacement
         war_per_90 = index - repl,
         war_per_90_lo = war_per_90 - z_range * se_per_90,
         war_per_90_hi = war_per_90 + z_range * se_per_90,
         war_lo     = war_per_90_lo * minutes_played / 90,
         war_hi     = war_per_90_hi * minutes_played / 90) %>%
  group_by(season_year) %>%
  mutate(war_rank = min_rank(desc(war))) %>%
  group_by(season_year, position) %>%
  mutate(war_rank_pos = min_rank(desc(war))) %>%
  ungroup() %>%
  arrange(season_year, war_rank) %>%
  select(season_year, conference, team, player_id, player_name, position, class_year,
         minutes_played, index_pctile, war, war_lo, war_hi, waa, war_per_90, war_per_90_lo, war_per_90_hi,
         se_per_90, war_rank, war_rank_pos, rpi_rank)

write_csv(out_cols(player_war), file.path(OUT_DIR, out_file("player_war.csv")))
cf_900 <- player_war %>% filter(position == "CF", between(minutes_played, 800, 1000))
cat(sprintf("Uncertainty: a CF with ~900 minutes has a %.0f%% range of about +/- %.3f %s per 90 (+/- %.2f %s)\n",
            100 * RANGE_LEVEL, median(z_range * cf_900$se_per_90), ABBR,
            median(z_range * cf_900$se_per_90 * cf_900$minutes_played / 90), UNIT))

# ---- 8b. How each player's rating adds up (the app's "How it's calculated" tab) --
# One row per player x stat. per_90 = category weight x share in the category x
# (his scaled stat - the position average): wins (points in points mode) per 90 above
# an average player at his position that season. Summed over a category's stats it
# gives weight x (category score - average); all rows plus the replacement gap
# (war_replacement.csv) add up to war_per_90. scaled is already flipped where fewer is better.
long %>%
  inner_join(cfg_cat %>% select(position, category, weight), by = c("position", "category")) %>%
  transmute(season_year, player_id, category, stat, direction, missing, raw, own_share, value, lo, hi,
            scaled = signed, scaled_avg = signed_avg, share, weight,
            per_90 = weight * share * (signed - signed_avg)) %>%
  arrange(season_year, player_id, category, stat) %>%
  mutate(across(where(is.double) & !season_year, ~ signif(.x, 5))) %>%
  write_csv(file.path(OUT_DIR, out_file("war_breakdown.csv")), na = "")
player_index %>%
  group_by(season_year, position) %>%
  summarise(avg_level = first(index_avg), repl_level = quantile(index[qualified], REPL_PCTL),
            qualified_players = sum(qualified), .groups = "drop") %>%
  mutate(avg_minus_repl = avg_level - repl_level) %>%
  write_csv(file.path(OUT_DIR, out_file("war_replacement.csv")))
tibble(setting = c("SHRINK_MODE", "PRIOR_MIN", "MIN_QUAL", "REPL_PCTL", "SCALE_LO", "SCALE_HI", "CAP_SCALE", "OUTCOME",
                   "WEIGHT_MODE", "TARGET", "OUT_PER_TARGET", "WEIGHTS_FILE", "HELDOUT_R2",
                   "NON_PENALTY", "ADJUST_SOS", "FIT_SEASONS", "FIT_TEAM_SEASONS"),
       value = c(SHRINK_MODE, PRIOR_MIN, MIN_QUAL, REPL_PCTL, SCALE_LO, SCALE_HI, CAP_SCALE, OUTCOME,
                 WEIGHT_MODE, TARGET, round(OUT_PER_TARGET, 4), WEIGHTS_FILE, round(r2_oos, 3),
                 NON_PENALTY, ADJUST_SOS,
                 paste(setdiff(sort(unique(teams$season_year)), FIT_EXCLUDE), collapse = ", "), nrow(fit_rows))) %>%
  write_csv(file.path(OUT_DIR, "model_settings.csv"))

team_war <- player_war %>%
  group_by(season_year, conference, team, rpi_rank) %>%
  summarise(team_war = sum(war), team_waa = sum(waa), .groups = "drop") %>%
  left_join(teams %>% select(season_year, team, games, win_pct, sos_pct),
            by = c("season_year", "team"))
write_csv(out_cols(team_war), file.path(OUT_DIR, out_file("team_war.csv")))

# ---- 9. Checks --------------------------------------------------------------

chk <- team_war %>% filter(!season_year %in% FIT_EXCLUDE)
cat(sprintf("Team %s vs team %s: r = %.2f\n", ABBR, if (OUTCOME == "points") "points per game" else "win %",
            cor(chk$team_war, chk$win_pct)))

yoy <- player_war %>%
  filter(minutes_played >= MIN_QUAL) %>%
  select(player_id, season_year, position, war_per_90) %>%
  inner_join(player_war %>% filter(minutes_played >= MIN_QUAL) %>%
               transmute(player_id, season_year = season_year - 1, next_war_per_90 = war_per_90),
             by = c("player_id", "season_year"))
yoy_r <- cor(yoy$war_per_90, yoy$next_war_per_90)
cat(sprintf("Year-to-year %s/90 for returning players: r = %.2f (n = %d)\n", ABBR, yoy_r, nrow(yoy)))

cat(sprintf("\nTop 10, %d:\n", LATEST))
print(player_war %>% filter(season_year == LATEST) %>% slice_head(n = 10) %>%
        select(player_name, team, position, minutes_played, war) %>% out_cols(), n = 10)

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

# In points mode, chart text says PAR and points instead of WAR and wins
# In points mode, chart text says PAR and points instead of WAR and wins. The text is
# changed as each chart is built (labs_pw below), which works in every ggplot2 version.
fix_text <- function(l) {
  if (OUTCOME != "points" || !is.character(l)) return(l)
  l <- gsub("WAR", "PAR", l)
  l <- gsub("Win % \\(ties = half\\)", "Points per game (3 for a win, 1 for a draw)", l)
  gsub("winning", "earning points", l)
}
labs_pw <- function(...) do.call(labs, lapply(list(...), fix_text))
# Charts are only drawn and saved when MAKE_PLOTS is TRUE (the slow part of a run)
save <- function(p, name, w = 9, h = 6) if (MAKE_PLOTS)
  ggsave(file.path(OUT_DIR, out_file(name)), p, width = w, height = h, dpi = 150)

# 01: top 25 players, latest season
top <- player_war %>% filter(season_year == LATEST) %>% slice_head(n = 25) %>%
  mutate(label = paste0(player_name, " (", team, ", ", position, ")"),
         group = factor(pos_group[position], levels = names(group_cols)))
p1 <- ggplot(top, aes(war, reorder(label, war), fill = group)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = group_cols) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs_pw(title = sprintf("Top 25 players by WAR, %d", LATEST),
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
  labs_pw(title = sprintf("%s WAR by position vs. top programs", FOCUS_TEAM),
       subtitle = sprintf("Average total WAR per team-season, %s", SEASON_SPAN),
       x = NULL, y = "WAR", caption = DATA_CAPTION) +
  theme_war + theme(panel.grid.major.x = element_blank())
save(p2, "02_team_vs_top_programs_by_position.png")

# 03: team WAR vs actual winning %
p3 <- ggplot(chk, aes(team_war, win_pct)) +
  geom_point(colour = "#2a78d6", alpha = 0.35, size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = ink2, linewidth = 0.6, formula = y ~ x) +
  geom_point(data = filter(chk, team == FOCUS_TEAM), colour = "#eb6834", size = 3) +
  scale_y_continuous(labels = if (OUTCOME == "points") label_number(accuracy = 0.1) else percent) +
  labs_pw(title = "Does team WAR line up with winning?",
       subtitle = sprintf("Each dot is a team-season; orange = %s. r = %.2f",
                          FOCUS_TEAM, cor(chk$team_war, chk$win_pct)),
       x = "Total team WAR", y = "Win % (ties = half)", caption = DATA_CAPTION) +
  theme_war
save(p3, "03_team_war_vs_win_pct.png")

# 04: does a player's WAR/90 carry over to next season?
p4 <- ggplot(yoy, aes(war_per_90 * 1000 / 90, next_war_per_90 * 1000 / 90)) +
  geom_point(colour = "#2a78d6", alpha = 0.15, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, colour = ink2, linewidth = 0.6, formula = y ~ x) +
  labs_pw(title = "Is WAR a repeatable skill?",
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
  labs_pw(title = "Does team WAR line up with RPI?",
       subtitle = sprintf("Each dot is a team-season; orange = %s. Spearman r = %.2f",
                          FOCUS_TEAM, rpi_r),
       x = "Total team WAR", y = "RPI rank", caption = DATA_CAPTION) +
  theme_war
save(p5, "05_team_war_vs_rpi.png")
cat(sprintf("Team %s vs RPI rank: Spearman r = %.2f\n", ABBR, rpi_r))

cat("\nDone. Outputs in", OUT_DIR, if (!MAKE_PLOTS) "(charts skipped: MAKE_PLOTS is FALSE)" else "", "\n")
