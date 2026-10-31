# =============================================================================
# Player GAR (goals above replacement) for NCAA D1 men's soccer: a net rating
#
# Same idea as player_war.R, but measured in goals and split into two parts:
#   ATTACK   the player's attacking stats, valued by how much they add to his
#            team's goals SCORED
#   DEFENSE  the player's defensive stats, valued by how much they cut his
#            team's goals CONCEDED (bad defending shows up as a negative number)
#
#   Net GAR = attack goals above average
#           + defense goals above average
#           + replacement value (how far an average player at the position is
#             above a replacement-level backup, for the minutes played)
#
# Inputs (in DATA_DIR):
#   simulated_player_metrics.csv   one row per player per season (Wyscout-style)
#   simulated_standings.csv        team records, goals for/against, schedule
#   gar_weights.csv                like war_weights_compiled.csv, plus a "side"
#                                  column: attack or defense
#
# Outputs (in OUT_DIR = GAR_output): player_gar.csv, team_gar.csv,
#   weights_used.csv, scorecard files, charts
#
# Steps 1-5 are the same as player_war.R (build stats, shrink, schedule
# adjustment). Unlike WAR, stats are NOT rescaled to 0-1: they stay in raw units.
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
OUT_DIR      <- "GAR_output"
STANDINGS_FILE <- "simulated_standings.csv"   # needs goals_for and goals_against columns
WEIGHTS_FILE <- "gar_weights.csv"   # stat list with a side column (attack / defense)
WEIGHT_MODE  <- "learned"      # "learned" or "manual"
MIN_QUAL     <- 300            # minutes to count toward scaling and replacement level
STAT_PRIOR_MIN <- c(prevented_goals = 1800)  # stats that need more minutes before they're
                               # trusted: goalkeeper shot-stopping swings a lot by luck, so a keeper
                               # is treated as if he also had 1,800 average minutes (PRIOR_MIN for others)
PRIOR_MIN    <- 270            # shrinkage: a player is treated as if he also played
                               # this many minutes at the position average
REPL_PCTL    <- 0.20           # replacement level = this percentile of qualified
                               # players' NET index at the position, each season
ADJUST_SOS   <- TRUE           # adjust stats for strength of schedule
FIT_EXCLUDE  <- c(2020)        # seasons left out of the weight fit (COVID)
FOCUS_TEAM   <- "Virginia Tech"
LATEST       <- 2025           # most recent complete season (for charts); NA = latest in the data
VERSION      <- ""             # scorecard label; "" = automatic from weights file and mode
SCORE_MIN    <- 600            # minutes needed to count in the scorecard and scatter
GOAL_UNIT_STATS <- c("xg", "xa")  # stats already measured in goals (expected goals and
                               # expected assists). They get ONE value per expected goal
                               # shared by every position, and the fit doesn't shrink it.
                               # character(0) = a separate value at each position
ATTACK_MODEL <- "chance_share"  # how attack is measured:
                               #   "chance_share" = each team's chances (its xG) are split between
                               #     the shooters and the passers who set them up (see 6b)
                               #   "learned" = old way: attacking stats fitted to team goals
PASSER_SHARE <- 0.40           # share of an assisted chance credited to the passer (xA);
                               # the shooter keeps the rest, plus all of his unassisted chances
FIXED_WEIGHTS <- c(prevented_goals = 1.0)  # goals per unit, set rather than fitted: a goal
                               # a keeper prevents is one goal fewer conceded (real data only)
XG_GOAL_VALUE <- 1.0           # goals per expected goal (xG is built so 1 xG = 1 goal on average)
RANGE_LEVEL  <- 0.80           # uncertainty range shown for each player (0.80 = the middle
                               # 80% of plausible values)

# Comparing players with our own roster (for the transfer portal)
ROSTER_SEASON <- NA            # season of our roster to compare everyone against; NA = LATEST
PROJ_MINUTES  <- 1500          # minutes a regular starter plays in a season
COMPARE_MIN   <- 600           # minutes a player needs to appear in the roster comparison
STARTER_SLOTS <- c(GK = 1, CB = 2, FB = 2, DM = 1, CM = 2, AM = 1, W = 2, CF = 1)
                               # how many of our players at each position count as regulars

# ---- Which data: simulated or real ------------------------------------------
# "simulated" = simulated_player_metrics.csv / simulated_standings.csv (many seasons)
# "real"      = the files prepare_real_data.R writes from the Wyscout export
#               (one season, conference games only). Run prepare_real_data.R first.
DATA_SOURCE  <- "real"
PLAYERS_FILE <- "simulated_player_metrics.csv"
if (DATA_SOURCE == "real") {
  PLAYERS_FILE   <- "real_player_metrics.csv"
  STANDINGS_FILE <- "real_standings.csv"
  WEIGHTS_FILE   <- "gar_weights_real.csv"   # stat list without stats the export lacks
  OUT_DIR        <- "GAR_output_real"
  ADJUST_SOS     <- FALSE   # the Adjusted sheet already corrects for conference strength
  LATEST         <- NA      # = the latest season in the file (2026)
  MIN_QUAL       <- 270     # a season so far is ~5-11 conference games, so lower minute cut-offs
  SCORE_MIN      <- 360
  COMPARE_MIN    <- 360
}

# Optional overrides from the command line:
#   Rscript player_gar.R gar_weights.csv manual
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) WEIGHTS_FILE <- args[1]
if (length(args) >= 2) WEIGHT_MODE  <- args[2]
if (length(args) >= 3) VERSION      <- args[3]

stopifnot(WEIGHT_MODE %in% c("learned", "manual"))
if (!file.exists(file.path(DATA_DIR, WEIGHTS_FILE))) stop("Weights file not found: ", WEIGHTS_FILE)
stopifnot(ATTACK_MODEL %in% c("chance_share", "learned"))
if (VERSION == "") VERSION <- sprintf("%s (%s, %s)", sub("\\.csv$", "", WEIGHTS_FILE), WEIGHT_MODE,
                                     if (ATTACK_MODEL == "chance_share")
                                       sprintf("chance share %d%%", round(100 * PASSER_SHARE))
                                     else "learned attack")
cat(sprintf("Weights: %s | mode: %s | scorecard label: %s\n", WEIGHTS_FILE, WEIGHT_MODE, VERSION))

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- 1. Load ----------------------------------------------------------------

players <- read_csv(file.path(DATA_DIR, PLAYERS_FILE),
                    show_col_types = FALSE) %>%
  filter(minutes_played > 0)

if (is.na(LATEST)) LATEST <- max(players$season_year)
if (is.na(ROSTER_SEASON)) ROSTER_SEASON <- LATEST
n_seasons <- n_distinct(players$season_year)
DATA_CAPTION <- if (DATA_SOURCE == "real") sprintf("Wyscout data, conference games, %d season to date", LATEST) else
  "Simulated player data"
span_yrs <- intersect(sort(unique(players$season_year)), setdiff((LATEST - 4):LATEST, FIT_EXCLUDE))
SEASON_SPAN <- if (length(span_yrs) == 1) as.character(span_yrs) else
  sprintf("%d-%d%s", min(span_yrs), max(span_yrs), if (any(FIT_EXCLUDE %in% min(span_yrs):max(span_yrs))) " (2020 excluded)" else "")
cat(sprintf("Data: %s (%d players, %d season%s; charts show %d)\n", PLAYERS_FILE, nrow(players),
            n_seasons, if (n_seasons == 1) "" else "s", LATEST))

standings <- read_csv(file.path(DATA_DIR, STANDINGS_FILE),
                      show_col_types = FALSE)
if (!all(c("goals_for", "goals_against") %in% names(standings)))
  stop(STANDINGS_FILE, " needs goals_for and goals_against columns for GAR")
standings <- standings %>%
  select(season_year, team, conference, rpi_rank, sos_rank,
         overall_w, overall_l, overall_t, goals_for, goals_against,
         any_of(c("games", "win_pct"))) %>%
  distinct(season_year, team, .keep_all = TRUE)

weights_cfg <- read_csv(file.path(DATA_DIR, WEIGHTS_FILE),
                        show_col_types = FALSE) %>%
  filter(direction != 0)
if (!"side" %in% names(weights_cfg) || !all(weights_cfg$side %in% c("attack", "defense")))
  stop(WEIGHTS_FILE, " needs a 'side' column with attack or defense on every row")
# With chance sharing, attack comes from xG and xA directly (step 6b), so the attack
# rows of the weights file aren't used; only the defense rows are fitted.
if (ATTACK_MODEL == "chance_share") weights_cfg <- filter(weights_cfg, side == "defense")

# games and win_pct can be given directly (real_standings.csv does: the export covers
# conference games only, so games come from team minutes); otherwise from the record
for (nm in c("games", "win_pct")) if (!nm %in% names(standings)) standings[[nm]] <- NA_real_

# ---- 2. Team context: record and schedule strength --------------------------

teams <- standings %>%
  mutate(games   = coalesce(games, overall_w + overall_l + overall_t),
         win_pct = coalesce(win_pct, (overall_w + 0.5 * overall_t) / (overall_w + overall_l + overall_t)),
         gf_pg   = goals_for / games,        # goals scored per game
         ga_pg   = goals_against / games) %>%  # goals conceded per game
  group_by(season_year) %>%
  # 1 = toughest schedule that season, 0 = easiest
  mutate(sos_pct = 1 - percent_rank(sos_rank)) %>%
  group_by(conference) %>%
  # rows without SOS (hand-entered 2018 ACC) get their conference's usual value
  mutate(sos_pct = coalesce(sos_pct, median(sos_pct, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(sos_pct = coalesce(sos_pct, 0.5)) %>%
  select(season_year, team, rpi_rank, games, win_pct, sos_pct, gf_pg, ga_pg)

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
  "xG against / 90",
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

# ---- 6. Position averages (stats stay in their raw units) -------------------
# No rescaling and no caps: every stat keeps its own units (per 90, %, ...), after
# the shrinkage and schedule adjustment above. Each player is compared with the
# minutes-weighted average at his position that season.

long <- long %>%
  group_by(season_year, position, stat) %>%
  mutate(val = value,
         pos_avg = weighted.mean(val, minutes_played)) %>%
  ungroup() %>%
  # how spread out each stat is at the position (for reading the weights and for
  # manual weights, which are set per standard deviation); no caps, context only
  group_by(position, stat) %>%
  mutate(pos_sd = sd(val[qualified], na.rm = TRUE)) %>%
  ungroup() %>%
  # goal-unit stats share one feature across positions ("all:xg"), so an expected
  # goal is worth the same whoever produces it
  mutate(feature = if_else(stat %in% GOAL_UNIT_STATS, paste0("all:", stat),
                           paste(position, stat, sep = ":")))


# ---- 6b. Attack by sharing each team's chances --------------------------------
# A team's chances are measured by its xG. Each chance is credited to the players
# who made it:
#   shooter: his xG, minus the passer's share on chances that were set up for him
#   passer:  PASSER_SHARE x his xA (the xG of the shots he set up)
# We don't know which of a shooter's shots were assisted, so each team's assisted
# share (team xA / team xG) is used for all its shooters. Summed over a team, the
# credits add back up to the team's xG, so nothing is counted twice.
# xG and xA get the same small-sample shrinkage and schedule adjustment as other stats.
if (ATTACK_MODEL == "chance_share") {
  ch <- players %>%
    select(all_of(id_cols), xg, xa, shots90 = `Shots / 90`, sa90 = `Shot assists / 90`) %>%
    mutate(shots90 = z(shots90), sa90 = z(sa90), qualified = minutes_played >= MIN_QUAL)
  assisted <- ch %>%
    group_by(season_year, team) %>%
    summarise(team_xg = sum(xg * minutes_played / 90), team_xa = sum(xa * minutes_played / 90),
              .groups = "drop") %>%
    mutate(assisted_share = pmin(1, if_else(team_xg > 0, team_xa / team_xg, 0)))
  ch <- ch %>%
    pivot_longer(c(xg, xa), names_to = "stat", values_to = "raw") %>%
    group_by(season_year, position, stat) %>%
    mutate(prior = mean(raw[qualified], na.rm = TRUE),
           value = (raw * minutes_played + prior * PRIOR_MIN) / (minutes_played + PRIOR_MIN)) %>%
    ungroup()
  if (ADJUST_SOS) {
    ch_fx <- ch %>%
      filter(qualified) %>%
      group_by(position, stat) %>%
      summarise(sos_slope = coef(lm(value ~ sos_pct + win_pct))[["sos_pct"]], .groups = "drop")
    ch <- ch %>%
      left_join(ch_fx, by = c("position", "stat")) %>%
      mutate(value = value - sos_slope * (sos_pct - 0.5))
  }
  chance_tab <- ch %>%
    select(player_id, season_year, team, position, minutes_played, stat, value, shots90, sa90) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    left_join(assisted %>% select(season_year, team, assisted_share), by = c("season_year", "team")) %>%
    mutate(shot_credit = xg * (1 - PASSER_SHARE * assisted_share),   # per 90
           pass_credit = PASSER_SHARE * xa,                            # per 90
           att         = XG_GOAL_VALUE * (shot_credit + pass_credit)) %>%
    group_by(season_year, position) %>%
    mutate(att_avg = weighted.mean(att, minutes_played)) %>%
    ungroup()
}

# ---- 7. Two weight fits: attack -> goals scored, defense -> goals conceded --
# Team-level feature for each position x stat, exactly as in player_war.R:
#   x = sum over the team's players of (stat value - position average) x minutes / (90 x games)
# Attack fit:   team goals SCORED per game    = start + sum(weight x x) + schedule
# Defense fit:  team goals PREVENTED per game = start + sum(weight x x) + schedule
#   "goals prevented" is minus goals conceded, so in both fits a positive weight
#   means "more of this stat is good", which matches the direction column.

if (!"side" %in% names(weights_cfg) || !all(weights_cfg$side %in% c("attack", "defense")))
  stop(WEIGHTS_FILE, " needs a 'side' column with attack or defense on every row")

team_features <- long %>%
  mutate(x = (val - pos_avg) * minutes_played / (90 * games)) %>%
  group_by(season_year, team, feature) %>%
  summarise(x = sum(x), .groups = "drop") %>%
  pivot_wider(names_from = feature, values_from = x, values_fill = 0) %>%
  inner_join(teams, by = c("season_year", "team"))

cfg <- weights_cfg %>%
  mutate(feature = if_else(stat %in% GOAL_UNIT_STATS, paste0("all:", stat),
                           paste(position, stat, sep = ":")),
         weight = NA_real_) %>%
  filter(feature %in% unique(long$feature))

fit_rows <- team_features %>%
  filter(!season_year %in% FIT_EXCLUDE, !is.na(gf_pg), !is.na(ga_pg))
targets      <- list(attack = fit_rows$gf_pg, defense = -fit_rows$ga_pg)
target_label <- c(attack = "goals scored", defense = "goals conceded")
r2_oos       <- c(attack = NA_real_, defense = NA_real_)

sides_fit <- if (ATTACK_MODEL == "chance_share") "defense" else c("attack", "defense")
for (sd in sides_fit) {
  cs    <- cfg$side == sd
  feats <- unique(cfg$feature[cs])
  dir_f <- cfg$direction[cs][match(feats, cfg$feature[cs])]
  y     <- targets[[sd]]
  if (WEIGHT_MODE == "learned") {
    X <- as.matrix(fit_rows[, c(feats, "sos_pct")])
    # direction is fixed by the weights file; the fit only picks the size
    lower <- c(ifelse(dir_f > 0, 0, -Inf), -Inf)
    upper <- c(ifelse(dir_f > 0, Inf, 0),  Inf)
    # leave one season out; with one season (real data), 10 random groups of teams
    folds <- if (n_distinct(fit_rows$season_year) >= 3) as.integer(factor(fit_rows$season_year)) else
      sample(rep_len(1:10, nrow(fit_rows)))
    cv <- cv.glmnet(X, y, alpha = 0, foldid = folds, keep = TRUE,
                    lower.limits = lower, upper.limits = upper,
                    # goal-unit stats and schedule aren't shrunk; everything else is
                    penalty.factor = c(ifelse(grepl("^all:", feats), 0, 1), 0))
    b <- coef(cv, s = "lambda.min")[, 1]
    cfg$weight[cs] <- unname(b[cfg$feature[cs]])
    pred_oos <- cv$fit.preval[, which(cv$lambda == cv$lambda.min)]
    r2_oos[[sd]] <- 1 - sum((y - pred_oos)^2) / sum((y - mean(y))^2)
    cat(sprintf("%s fit: team %s per game explained, held-out data: %.2f\n",
                tools::toTitleCase(sd), target_label[[sd]], r2_oos[[sd]]))
  } else {
    # manual_weight = goals-scale importance per 1 standard deviation of the stat at the
    # position (so a % stat and a per-90 count are comparable); xG/xA: per 1.0 expected goal
    sds <- long %>% distinct(position, stat, pos_sd)
    sd_cs <- sds$pos_sd[match(paste(cfg$position[cs], cfg$stat[cs]), paste(sds$position, sds$stat))]
    per_unit <- ifelse(cfg$stat[cs] %in% GOAL_UNIT_STATS, 1, 1 / pmax(sd_cs, 1e-9))
    cfg$weight[cs] <- cfg$direction[cs] * cfg$manual_weight[cs] * per_unit
  }
}

# Stats already measured in goals get their weight set, not fitted
fx <- cfg$stat %in% names(FIXED_WEIGHTS)
if (any(fx) && WEIGHT_MODE == "learned") cfg$weight[fx] <- unname(FIXED_WEIGHTS[cfg$stat[fx]])

if (ATTACK_MODEL == "chance_share") {
  # check: do team chances (xG) line up with team goals? Uses the raw totals.
  chk_xg <- assisted %>%
    inner_join(teams, by = c("season_year", "team")) %>%
    filter(!season_year %in% FIT_EXCLUDE, !is.na(gf_pg)) %>%
    mutate(xg_pg = team_xg / games)
  m_xg <- lm(gf_pg ~ xg_pg, data = chk_xg)
  r2_oos[["attack"]] <- summary(m_xg)$r.squared
  cat(sprintf(paste0("Attack (chance sharing, %d%% to the passer): team xG explains %.2f of team goals ",
                     "scored; 1 xG = %.2f goals (XG_GOAL_VALUE uses %.2f)\n"),
              round(100 * PASSER_SHARE), r2_oos[["attack"]], coef(m_xg)[["xg_pg"]], XG_GOAL_VALUE))
}

# Position calibration (manual mode only): converts each position's attack and
# defense index into goals. In learned mode the weights are already in goals.
if (WEIGHT_MODE == "learned") {
  calib <- cfg %>% distinct(position, side) %>% mutate(beta = 1)
} else {
  pos_feats <- long %>%
    left_join(cfg %>% select(position, stat, side, weight), by = c("position", "stat")) %>%
    mutate(x = weight * (val - pos_avg) * minutes_played / (90 * games)) %>%
    group_by(season_year, team, side, position) %>%
    summarise(x = sum(x), .groups = "drop")
  calib <- map_dfr(sides_fit, function(sd) {
    dd <- pos_feats %>%
      filter(side == sd) %>%
      pivot_wider(names_from = position, values_from = x, values_fill = 0, names_prefix = "pos_") %>%
      inner_join(teams, by = c("season_year", "team")) %>%
      filter(!season_year %in% FIT_EXCLUDE, !is.na(gf_pg))
    pos_cols <- grep("^pos_", names(dd), value = TRUE)
    dd$y <- if (sd == "attack") dd$gf_pg else -dd$ga_pg
    m <- lm(reformulate(c(pos_cols, "sos_pct"), "y"), data = dd)
    cat(sprintf("%s (manual weights, in-sample R-squared): %.2f\n",
                tools::toTitleCase(sd), summary(m)$r.squared))
    tibble(side = sd, position = sub("^pos_", "", pos_cols), beta = unname(coef(m)[pos_cols]))
  })
  if (any(calib$beta <= 0, na.rm = TRUE))
    warning("Some position/side combinations got a negative calibration; check manual weights: ",
            paste(with(filter(calib, beta <= 0), paste(position, side)), collapse = ", "))
}

pct_stats <- c("def_duel_win_pct", "aerial_win_pct", "pass_acc", "duel_win_pct", "save_pct")
weights_used <- cfg %>%
  left_join(calib, by = c("position", "side")) %>%
  left_join(long %>% distinct(position, stat, pos_sd), by = c("position", "stat")) %>%
  mutate(goal_weight = weight * beta,
         # goal_weight = goals added (attack) or prevented (defense) per 90 for each
         # 1 unit of the stat above the position average
         units = case_when(stat %in% GOAL_UNIT_STATS ~ "goals per 1.0 expected goal",
                           stat %in% pct_stats       ~ "goals per 90 per percentage point",
                           TRUE                      ~ "goals per 90 per 1.0 of the stat (per 90)"),
         # to compare stats: a player 1 standard deviation above the position average
         # over a full season (1,800 minutes) adds this many goals
         goals_per_season_1sd = goal_weight * pos_sd * 1800 / 90) %>%
  select(position, side, stat, units, direction, goal_weight, stat_sd_at_position = pos_sd,
         goals_per_season_1sd, note) %>%
  arrange(position, side, desc(abs(goals_per_season_1sd)))
if (ATTACK_MODEL == "chance_share") {
  weights_used <- bind_rows(
    tibble(position = "all", side = "attack", stat = c("xg (shooter)", "xa (passer)"),
           units = "goals per expected goal", direction = 1,
           goal_weight = XG_GOAL_VALUE * c(1 - PASSER_SHARE * mean(assisted$assisted_share), PASSER_SHARE),
           note = c(sprintf(paste0("shooter keeps all of his unassisted chances and %d%% of assisted ones; ",
                                   "%.2f = league average (each team's own assisted share is used)"),
                            round(100 * (1 - PASSER_SHARE)),
                            1 - PASSER_SHARE * mean(assisted$assisted_share)),
                    sprintf("passer gets %d%% of the xG of every shot he sets up", round(100 * PASSER_SHARE)))),
    weights_used)
}
write_csv(weights_used, file.path(OUT_DIR, "weights_used.csv"))

# ---- 8. Attack index, defense index, replacement level, GAR -----------------

stat_sums <- long %>%
  left_join(weights_used %>% select(position, stat, side, goal_weight), by = c("position", "stat")) %>%
  group_by(player_id, season_year) %>%
  summarise(att     = sum(goal_weight * val     * (side == "attack")),
            att_avg = sum(goal_weight * pos_avg * (side == "attack")),
            def     = sum(goal_weight * val     * (side == "defense")),
            def_avg = sum(goal_weight * pos_avg * (side == "defense")),
            .groups = "drop")

# every player who played, including positions with no defensive stats (W, CF)
player_index <- players %>%
  select(all_of(id_cols)) %>%
  mutate(qualified = minutes_played >= MIN_QUAL) %>%
  left_join(stat_sums, by = c("player_id", "season_year")) %>%
  mutate(across(c(att, att_avg, def, def_avg), ~ coalesce(.x, 0)))
if (ATTACK_MODEL == "chance_share") {
  player_index <- player_index %>%
    select(-att, -att_avg) %>%
    left_join(chance_tab %>% select(player_id, season_year, att, att_avg),
              by = c("player_id", "season_year"))
}
player_index <- player_index %>% mutate(net = att + def, net_avg = att_avg + def_avg)

# Goals -> wins: how many extra wins one goal of goal difference is worth
gw_rows <- filter(teams, !season_year %in% FIT_EXCLUDE, !is.na(gf_pg))
WINS_PER_GOAL <- coef(lm(win_pct ~ I(gf_pg - ga_pg), data = gw_rows))[[2]]
cat(sprintf("One goal of goal difference is worth about %.2f wins (%.1f goals per win)\n",
            WINS_PER_GOAL, 1 / WINS_PER_GOAL))

player_gar <- player_index %>%
  group_by(season_year, position) %>%
  mutate(repl = quantile(net[qualified], REPL_PCTL)) %>%
  ungroup() %>%
  mutate(mins90     = minutes_played / 90,
         off_gaa    = (att - att_avg) * mins90,    # attack: goals above an average player
         def_gaa    = (def - def_avg) * mins90,    # defense: goals prevented vs an average player
         repl_value = (net_avg - repl) * mins90,   # average player vs replacement, for his minutes
         gar        = (net - repl) * mins90,       # = off_gaa + def_gaa + repl_value
         gar_per_90 = net - repl,
         off_per_90 = att - att_avg,
         def_per_90 = def - def_avg,
         war_equiv  = gar * WINS_PER_GOAL) %>%
  group_by(season_year) %>%
  mutate(gar_rank = min_rank(desc(gar))) %>%
  group_by(season_year, position) %>%
  mutate(gar_rank_pos = min_rank(desc(gar))) %>%
  ungroup() %>%
  arrange(season_year, gar_rank) %>%
  select(season_year, conference, team, player_id, player_name, position, class_year,
         minutes_played, gar, off_gaa, def_gaa, repl_value, gar_per_90, off_per_90, def_per_90,
         war_equiv, gar_rank, gar_rank_pos, rpi_rank)

# ---- 8b. Uncertainty ranges ------------------------------------------------
# How much a rating could be off because a season is a limited sample. Each stat's
# sampling noise is estimated from the stat itself:
#   counts per 90 (crosses, interceptions, ...)  behave like counts: variance ~ rate
#   percentages (duel win %, pass accuracy)      depend on attempts: p(100 - p) / attempts
#   xG and xA                                    depend on how many shots / chances they
#                                                come from (a few big chances = noisier)
# The noise shrinks with minutes, and the same shrinkage toward the position average
# as step 4 is applied (PRIOR_MIN). Stats are combined using their weights:
#   noise of the rating = sqrt( sum of weight^2 x noise of each stat )
# Stats are treated as independent, so ranges are a little narrow if stats move together.
z_range <- qnorm(0.5 + RANGE_LEVEL / 2)
helper <- players %>%
  transmute(player_id, season_year,
            vol_def  = z(`Defensive duels / 90 minutes`), vol_aer = z(`Aerial duels / 90 minutes`),
            vol_pass = z(`Passes / 90`), shots90 = z(`Shots / 90`), sa90 = z(`Shot assists / 90`),
            xga90 = z(`xG against / 90`))
unc <- long %>%
  inner_join(weights_used %>% select(position, stat, goal_weight), by = c("position", "stat")) %>%
  left_join(helper, by = c("player_id", "season_year")) %>%
  mutate(attempts = case_when(stat == "def_duel_win_pct" ~ vol_def,
                              stat == "aerial_win_pct"   ~ vol_aer,
                              stat == "pass_acc"         ~ vol_pass),
         # noise of one season's per-90 value = c x 90 / minutes
         c = case_when(!is.na(attempts) ~ pmax(value, 0) * pmax(100 - value, 0) / pmax(attempts, 0.5),
                       stat == "xg" ~ 2 * pmax(value, 0) * pmin(pmax(value, 0) / pmax(shots90, 0.1), 0.6),
                       stat == "xa" ~ 2 * pmax(value, 0) * pmin(pmax(value, 0) / pmax(sa90, 0.1), 0.6),
                       stat == "prevented_goals" ~ xga90,   # goals conceded vary like a count
                       TRUE ~ pmax(value, 0)),
         post_var   = c * 90 / (minutes_played + pm),                    # after shrinkage
         stat_var = post_var) %>%
  group_by(player_id, season_year) %>%
  summarise(stat_var_sum = sum(goal_weight^2 * stat_var, na.rm = TRUE), .groups = "drop")

# chance-sharing attack: noise of xG and xA, weighted by the shooter / passer shares
att_var <- if (ATTACK_MODEL == "chance_share") {
  chance_tab %>%
    mutate(v_xg = 2 * pmax(xg, 0) * pmin(pmax(xg, 0) / pmax(shots90, 0.1), 0.6) * 90 / (minutes_played + PRIOR_MIN),
           v_xa = 2 * pmax(xa, 0) * pmin(pmax(xa, 0) / pmax(sa90, 0.1), 0.6) * 90 / (minutes_played + PRIOR_MIN),
           att_var = XG_GOAL_VALUE^2 * ((1 - PASSER_SHARE * assisted_share)^2 * v_xg + PASSER_SHARE^2 * v_xa)) %>%
    select(player_id, season_year, att_var)
} else tibble(player_id = character(), season_year = numeric(), att_var = numeric())

unc <- player_index %>%
  distinct(player_id, season_year) %>%
  left_join(unc, by = c("player_id", "season_year")) %>%
  left_join(att_var, by = c("player_id", "season_year")) %>%
  mutate(se_per_90 = sqrt(coalesce(stat_var_sum, 0) + coalesce(att_var, 0))) %>%
  select(player_id, season_year, se_per_90)

player_gar <- player_gar %>%
  left_join(unc, by = c("player_id", "season_year")) %>%
  mutate(gar_per_90_lo = gar_per_90 - z_range * se_per_90,
         gar_per_90_hi = gar_per_90 + z_range * se_per_90,
         gar_lo        = gar_per_90_lo * minutes_played / 90,
         gar_hi        = gar_per_90_hi * minutes_played / 90) %>%
  relocate(gar_lo, gar_hi, .after = gar)
cf_900 <- player_gar %>% filter(position == "CF", between(minutes_played, 800, 1000))
cat(sprintf("Uncertainty: a CF with ~900 minutes has a %.0f%% range of about +/- %.3f GAR per 90 (+/- %.1f goals)\n",
            100 * RANGE_LEVEL, median(z_range * cf_900$se_per_90),
            median(z_range * cf_900$se_per_90 * cf_900$minutes_played / 90)))

write_csv(player_gar, file.path(OUT_DIR, "player_gar.csv"))

# ---- 8c. Compare every player with our own roster ---------------------------
# For the portal, the real alternative to a target is the player we already have.
# Our "regulars" at a position = our players with the most minutes in ROSTER_SEASON,
# up to STARTER_SLOTS (e.g. 2 CBs). A target is compared with our WEAKEST regular at
# his position (the spot he would most likely take).
our_roster <- player_gar %>%
  filter(team == FOCUS_TEAM, season_year == ROSTER_SEASON) %>%
  group_by(position) %>%
  arrange(desc(minutes_played), .by_group = TRUE) %>%
  mutate(slot = row_number(), regular = slot <= STARTER_SLOTS[position[1]]) %>%
  ungroup()
write_csv(our_roster %>% select(position, slot, regular, player_name, class_year, minutes_played,
                                gar, gar_lo, gar_hi, gar_per_90, gar_per_90_lo, gar_per_90_hi),
          file.path(OUT_DIR, "our_roster.csv"))

benchmark <- our_roster %>%
  filter(regular) %>%
  group_by(position) %>%
  slice_min(gar_per_90, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(position, benchmark_player = player_name, benchmark_per_90 = gar_per_90,
            benchmark_se = se_per_90)

portal_vs_roster <- player_gar %>%
  filter(season_year == ROSTER_SEASON, team != FOCUS_TEAM, minutes_played >= COMPARE_MIN) %>%
  inner_join(benchmark, by = "position") %>%
  mutate(se_diff        = sqrt(se_per_90^2 + benchmark_se^2),
         upgrade_per_90 = gar_per_90 - benchmark_per_90,
         # goals over a full season as a starter (PROJ_MINUTES), with its range
         season_upgrade    = upgrade_per_90 * PROJ_MINUTES / 90,
         season_upgrade_lo = (upgrade_per_90 - z_range * se_diff) * PROJ_MINUTES / 90,
         season_upgrade_hi = (upgrade_per_90 + z_range * se_diff) * PROJ_MINUTES / 90,
         chance_better     = pnorm(upgrade_per_90 / se_diff)) %>%
  arrange(position, desc(gar_per_90)) %>%
  select(position, player_name, team, conference, class_year, minutes_played,
         gar_per_90, gar_per_90_lo, gar_per_90_hi, benchmark_player, benchmark_per_90,
         upgrade_per_90, season_upgrade, season_upgrade_lo, season_upgrade_hi, chance_better, rpi_rank)
write_csv(portal_vs_roster, file.path(OUT_DIR, "portal_vs_roster.csv"))

team_gar <- player_gar %>%
  group_by(season_year, conference, team, rpi_rank) %>%
  summarise(team_gar = sum(gar), team_off_gaa = sum(off_gaa), team_def_gaa = sum(def_gaa),
            .groups = "drop") %>%
  left_join(teams %>% select(season_year, team, games, win_pct, gf_pg, ga_pg, sos_pct),
            by = c("season_year", "team")) %>%
  mutate(gd_pg = gf_pg - ga_pg)
write_csv(team_gar, file.path(OUT_DIR, "team_gar.csv"))

# ---- 9. Checks --------------------------------------------------------------

chk <- team_gar %>% filter(!season_year %in% FIT_EXCLUDE, !is.na(gf_pg))
r_att <- cor(chk$team_off_gaa / chk$games, chk$gf_pg)
r_def <- cor(chk$team_def_gaa / chk$games, chk$ga_pg)
r_net <- cor(chk$team_gar / chk$games, chk$gd_pg)
cat(sprintf("Team attack GAA vs goals scored per game:   r = %.2f\n", r_att))
cat(sprintf("Team defense GAA vs goals conceded per game: r = %.2f (negative = good)\n", r_def))
cat(sprintf("Team GAR vs goal difference per game:        r = %.2f\n", r_net))

yoy <- player_gar %>%
  filter(minutes_played >= MIN_QUAL) %>%
  select(player_id, season_year, gar_per_90) %>%
  inner_join(player_gar %>% filter(minutes_played >= MIN_QUAL) %>%
               transmute(player_id, season_year = season_year - 1, next_gar_per_90 = gar_per_90),
             by = c("player_id", "season_year"))
yoy_r <- cor(yoy$gar_per_90, yoy$next_gar_per_90)
cat(sprintf("Year-to-year GAR/90 for returning players: r = %.2f (n = %d)\n", yoy_r, nrow(yoy)))

cat(sprintf("\nTop 10, %d:\n", LATEST))
print(player_gar %>% filter(season_year == LATEST) %>% slice_head(n = 10) %>%
        select(player_name, team, position, minutes_played, gar, off_gaa, def_gaa, repl_value) %>%
        mutate(across(where(is.double), ~ round(.x, 2))), n = 10)

# ---- 10. Charts -------------------------------------------------------------

pos_order  <- c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")
pos_group  <- c(GK = "Goalkeeper", CB = "Defender", FB = "Defender",
                DM = "Midfielder", CM = "Midfielder", AM = "Midfielder",
                W = "Attacker", CF = "Attacker")
group_cols <- c(Goalkeeper = "#7b5cd6", Defender = "#2a78d6",
                Midfielder = "#1baf7a", Attacker = "#eda100")
side_cols  <- c(Attack = "#eb6834", Defense = "#2a78d6", `Average over replacement` = "#c9c7c1")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"

theme_gar <- theme_minimal(base_size = 12) +
  theme(plot.background  = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
        plot.title       = element_text(face = "bold", colour = ink),
        plot.subtitle    = element_text(colour = ink2),
        plot.caption     = element_text(colour = ink2, size = 9),
        axis.text        = element_text(colour = ink2),
        axis.title       = element_text(colour = ink2),
        strip.text       = element_text(colour = ink, face = "bold", hjust = 0),
        legend.position  = "top", legend.justification = "left",
        legend.title     = element_blank())

save <- function(p, name, w = 9, h = 6) ggsave(file.path(OUT_DIR, name), p, width = w, height = h, dpi = 150)

# 01: top 25 players, with where their GAR comes from
top <- player_gar %>%
  filter(season_year == LATEST) %>%
  slice_min(gar_rank, n = 25, with_ties = FALSE) %>%
  mutate(label = paste0(player_name, " (", team, ", ", position, ")"),
         label = factor(label, levels = rev(label)))
top_parts <- top %>%
  select(label, Attack = off_gaa, Defense = def_gaa, `Average over replacement` = repl_value) %>%
  pivot_longer(-label, names_to = "part", values_to = "goals") %>%
  mutate(part = factor(part, levels = rev(names(side_cols))))
p1 <- ggplot(top_parts, aes(goals, label, fill = part)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = ink2, linewidth = 0.4) +
  geom_errorbarh(data = top, aes(xmin = gar_lo, xmax = gar_hi, y = label), inherit.aes = FALSE,
                 height = 0.35, colour = ink, linewidth = 0.45) +
  geom_point(data = top, aes(gar, label), inherit.aes = FALSE,
             shape = 21, fill = "white", colour = ink, size = 2.3, stroke = 0.8) +
  scale_fill_manual(values = side_cols, breaks = names(side_cols)) +
  labs(title = sprintf("Top 25 players by GAR, %d", LATEST),
       subtitle = sprintf("Goals above replacement: attack + defense + replacement value\nDot = net GAR; line = %d%% range", round(100 * RANGE_LEVEL)),
       x = "Goals", y = NULL, caption = DATA_CAPTION) +
  theme_gar + theme(panel.grid.major.y = element_blank())
save(p1, "01_top25_gar.png", w = 10, h = 8)

# 01b: the same chart for each position, in GAR_output/by_position/
pos_names <- c(GK = "goalkeepers", CB = "center backs", FB = "fullbacks",
               DM = "defensive midfielders", CM = "central midfielders",
               AM = "attacking midfielders", W = "wingers", CF = "center forwards")
dir.create(file.path(OUT_DIR, "by_position"), showWarnings = FALSE)
for (ps in pos_order) {
  top_p <- player_gar %>%
    filter(season_year == LATEST, position == ps) %>%
    slice_min(gar_rank_pos, n = 25, with_ties = FALSE) %>%
    mutate(label = paste0(player_name, " (", team, ")"),
           label = factor(label, levels = rev(unique(label))))
  if (nrow(top_p) == 0) next
  parts_p <- top_p %>%
    select(label, Attack = off_gaa, Defense = def_gaa, `Average over replacement` = repl_value) %>%
    pivot_longer(-label, names_to = "part", values_to = "goals") %>%
    mutate(part = factor(part, levels = rev(names(side_cols))))
  pp <- ggplot(parts_p, aes(goals, label, fill = part)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = ink2, linewidth = 0.4) +
    geom_errorbarh(data = top_p, aes(xmin = gar_lo, xmax = gar_hi, y = label), inherit.aes = FALSE,
                   height = 0.35, colour = ink, linewidth = 0.45) +
    geom_point(data = top_p, aes(gar, label), inherit.aes = FALSE,
               shape = 21, fill = "white", colour = ink, size = 2.3, stroke = 0.8) +
    scale_fill_manual(values = side_cols, breaks = names(side_cols), drop = FALSE) +
    labs(title = sprintf("Top %d %s by GAR, %d", nrow(top_p), pos_names[[ps]], LATEST),
         subtitle = sprintf("Goals above replacement: attack + defense + replacement value\nDot = net GAR; line = %d%% range", round(100 * RANGE_LEVEL)),
         x = "Goals", y = NULL, caption = DATA_CAPTION) +
    theme_gar + theme(panel.grid.major.y = element_blank())
  save(pp, file.path("by_position", sprintf("01_top25_gar_%s.png", ps)), w = 10, h = 8)
}

# 01c: who would upgrade our weakest regular at each position? (GAR_output/vs_roster/)
dir.create(file.path(OUT_DIR, "vs_roster"), showWarnings = FALSE)
chance_cols <- c("Very likely better (90%+)" = "#1baf7a", "Likely better (70-90%)" = "#7fcfa9",
                 "Unclear (30-70%)" = "#a8a6a0", "Likely not better (<30%)" = "#eb6834")
for (ps in pos_order) {
  bm <- filter(benchmark, position == ps)
  ours <- filter(our_roster, position == ps, regular)
  tg <- portal_vs_roster %>%
    filter(position == ps) %>%
    slice_max(gar_per_90, n = 20, with_ties = FALSE) %>%
    mutate(verdict = cut(chance_better, c(-Inf, 0.3, 0.7, 0.9, Inf),
                         labels = rev(names(chance_cols))),
           verdict = factor(verdict, levels = names(chance_cols)),
           label = sprintf("%s (%s, %s)  %d%% | %+.1f", player_name, team, class_year,
                           round(100 * chance_better), season_upgrade),
           label = factor(label, levels = rev(label)))
  if (nrow(tg) == 0 || nrow(bm) == 0) next
  band <- with(ours %>% filter(player_name == bm$benchmark_player),
               c(gar_per_90_lo[1], gar_per_90_hi[1]))
  pv <- ggplot(tg, aes(gar_per_90, label)) +
    annotate("rect", xmin = band[1], xmax = band[2], ymin = -Inf, ymax = Inf,
             fill = "#861F41", alpha = 0.08) +
    geom_vline(data = ours, aes(xintercept = gar_per_90), colour = "#861F41",
               linetype = ifelse(ours$player_name == bm$benchmark_player, "solid", "dashed"),
               linewidth = 0.6) +
    geom_text(data = ours, aes(x = gar_per_90, y = Inf, label = sub(".* ", "", player_name)),
              inherit.aes = FALSE, colour = "#861F41", size = 3.2, vjust = -0.4, hjust = 0.5) +
    geom_errorbarh(aes(xmin = gar_per_90_lo, xmax = gar_per_90_hi, colour = verdict),
                   height = 0.35, linewidth = 0.6) +
    geom_point(aes(colour = verdict), size = 2.6) +
    scale_colour_manual(values = chance_cols, drop = FALSE) +
    guides(colour = guide_legend(nrow = 2)) +
    coord_cartesian(clip = "off") +
    labs(title = sprintf("Who would upgrade %s's %s? %d", FOCUS_TEAM, pos_names[[ps]], ROSTER_SEASON),
         subtitle = sprintf(paste0("Top %d by GAR per 90 (%d+ minutes), with %d%% range. Maroon lines = our regulars;\n",
                                   "solid line + shading = our weakest regular, %s, and his range.\n",
                                   "Label: chance he is better than %s | goals gained over a %s-minute season"),
                            nrow(tg), COMPARE_MIN, round(100 * RANGE_LEVEL), bm$benchmark_player,
                            sub(".* ", "", bm$benchmark_player), format(PROJ_MINUTES, big.mark = ",")),
         x = "GAR per 90 (goals above replacement)", y = NULL, caption = DATA_CAPTION) +
    theme_gar + theme(panel.grid.major.y = element_blank(), plot.margin = margin(12, 12, 6, 6))
  save(pv, file.path("vs_roster", sprintf("upgrade_%s.png", ps)), w = 10, h = 8)
}

# 02: attack vs defense for every regular, latest season
sc <- player_gar %>%
  filter(season_year == LATEST, minutes_played >= SCORE_MIN) %>%
  mutate(group = factor(pos_group[position], levels = names(group_cols)))
focus_pts <- filter(sc, team == FOCUS_TEAM) %>%
  mutate(short = paste0(sub(".* ", "", player_name), " (", position, ")"))
lims <- max(abs(c(sc$off_per_90, sc$def_per_90))) * 1.05
p2 <- ggplot(sc, aes(off_per_90, def_per_90)) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = ink2, linewidth = 0.4) +
  geom_point(aes(colour = group), alpha = 0.45, size = 1.7) +
  geom_point(data = focus_pts, shape = 21, fill = "#861F41", colour = "white", size = 3.2, stroke = 0.7) +
  geom_text(data = focus_pts, aes(label = short), size = 3, colour = "#861F41",
            hjust = -0.12, vjust = 0.4, check_overlap = TRUE) +
  annotate("text", x =  lims, y =  lims, label = "Helps at both ends",  hjust = 1, vjust = 1, colour = ink2, size = 3.3) +
  annotate("text", x = -lims, y =  lims, label = "Defense-first",       hjust = 0, vjust = 1, colour = ink2, size = 3.3) +
  annotate("text", x =  lims, y = -lims, label = "Attack-first",        hjust = 1, vjust = 0, colour = ink2, size = 3.3) +
  annotate("text", x = -lims, y = -lims, label = "Below average at both", hjust = 0, vjust = 0, colour = ink2, size = 3.3) +
  scale_colour_manual(values = group_cols) +
  coord_cartesian(xlim = c(-lims, lims), ylim = c(-lims, lims)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(title = sprintf("Attack vs defense, %d", LATEST),
       subtitle = sprintf("Goals above an average player at the same position, per 90\nPlayers with %d+ minutes; maroon = %s",
                          SCORE_MIN, FOCUS_TEAM),
       x = "Attack (goals added per 90)", y = "Defense (goals prevented per 90)",
       caption = DATA_CAPTION) +
  theme_gar
save(p2, "02_attack_vs_defense.png", w = 9, h = 8)

# 03: focus team vs Champion (RPI 1-10) and Elite (RPI 11-25) teams, by position and side
tier_levels <- c(FOCUS_TEAM, "Champion tier (RPI 1-10)", "Elite tier (RPI 11-25)")
by_pos <- player_gar %>%
  mutate(group = case_when(team == FOCUS_TEAM ~ FOCUS_TEAM,
                           rpi_rank <= 10 ~ "Champion tier (RPI 1-10)",
                           rpi_rank <= 25 ~ "Elite tier (RPI 11-25)")) %>%
  filter(!is.na(group), !season_year %in% FIT_EXCLUDE, season_year >= LATEST - 4) %>%
  group_by(season_year, team, group, position) %>%
  summarise(Attack = sum(off_gaa), Defense = sum(def_gaa), .groups = "drop") %>%
  complete(nesting(season_year, team, group), position, fill = list(Attack = 0, Defense = 0)) %>%
  pivot_longer(c(Attack, Defense), names_to = "side", values_to = "goals") %>%
  group_by(group, position, side) %>%
  summarise(goals = mean(goals), .groups = "drop") %>%
  mutate(position = factor(position, levels = pos_order),
         group = factor(group, levels = tier_levels))
p3 <- ggplot(by_pos, aes(position, goals, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_hline(yintercept = 0, colour = ink2, linewidth = 0.4) +
  facet_wrap(~ side, ncol = 1) +
  scale_fill_manual(values = c("#861F41", "#eb6834", "#1baf7a")) +
  labs(title = sprintf("%s vs. top programs: attack and defense by position", FOCUS_TEAM),
       subtitle = sprintf("Average goals above an average player, per team-season, %s", SEASON_SPAN),
       x = NULL, y = "Goals", caption = DATA_CAPTION) +
  theme_gar + theme(panel.grid.major.x = element_blank())
save(p3, "03_team_vs_top_programs.png", h = 7)

# 04: do team attack and defense ratings line up with goals scored and conceded?
team_long <- bind_rows(
  chk %>% transmute(panel = sprintf("Attack: goals scored per game (r = %.2f)", r_att),
                    rating = team_off_gaa / games, actual = gf_pg, team, season_year),
  chk %>% transmute(panel = sprintf("Defense: goals conceded per game (r = %.2f)", r_def),
                    rating = team_def_gaa / games, actual = ga_pg, team, season_year))
p4 <- ggplot(team_long, aes(rating, actual)) +
  geom_point(colour = "#2a78d6", alpha = 0.3, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, colour = ink2, linewidth = 0.6, formula = y ~ x) +
  geom_point(data = filter(team_long, team == FOCUS_TEAM), colour = "#861F41", size = 2.6) +
  facet_wrap(~ panel, scales = "free") +
  labs(title = "Do team ratings line up with goals?",
       subtitle = sprintf("Each dot is a team-season; maroon = %s. Defense should slope down: better rating, fewer conceded",
                          FOCUS_TEAM),
       x = "Team rating per game (sum of players' goals above average / games)", y = "Actual goals per game",
       caption = DATA_CAPTION) +
  theme_gar
save(p4, "04_team_ratings_vs_goals.png", w = 11, h = 5.5)

# ---- 11. Scorecard ----------------------------------------------------------
# Same as player_war.R, on GAR per 90. Skipped automatically on real data.

if (all(c("true_talent", "team_strength_z") %in% names(players))) {

  sc_data <- player_gar %>%
    filter(minutes_played >= SCORE_MIN, !season_year %in% FIT_EXCLUDE) %>%
    inner_join(players %>% select(player_id, season_year, true_talent, team_strength_z),
               by = c("player_id", "season_year"))

  partial_r <- function(x, y, z) cor(resid(lm(x ~ z)), resid(lm(y ~ z)))

  by_position <- sc_data %>%
    group_by(position) %>%
    summarise(n                = n(),
              r_talent         = cor(gar_per_90, true_talent),
              r_talent_no_team = partial_r(gar_per_90, true_talent, team_strength_z),
              r_team           = cor(gar_per_90, team_strength_z),
              talent_vs_team   = cor(true_talent, team_strength_z),
              .groups = "drop")

  scorecard <- bind_rows(
    by_position,
    by_position %>%
      summarise(position = "Average", n = sum(n),
                across(c(r_talent, r_talent_no_team, r_team, talent_vs_team), mean))
  ) %>%
    pivot_longer(-c(position, n), names_to = "metric", values_to = "value") %>%
    bind_rows(tibble(
      position = "Model",
      n        = NA_integer_,
      metric   = c("heldout_r2_attack", "heldout_r2_defense", "year_to_year_r", "team_r_goal_diff"),
      value    = c(r2_oos[["attack"]], r2_oos[["defense"]], yoy_r, r_net))) %>%
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

  # 05: this run, by position
  metric_labels <- c(r_talent_no_team = "GAR vs true talent, team strength removed (main score)",
                     r_talent         = "GAR vs true talent",
                     r_team           = "GAR vs team strength",
                     talent_vs_team   = "True talent vs team strength (expected team link)")
  metric_cols   <- c("#1baf7a", "#2a78d6", "#eb6834", "#a8a6a0")
  sc_plot <- scorecard %>%
    filter(metric %in% names(metric_labels)) %>%
    mutate(position = factor(position, levels = rev(c(pos_order, "Average"))),
           metric   = factor(metric_labels[metric], levels = metric_labels))
  mv <- with(filter(scorecard, position == "Model"), setNames(value, metric))
  fmt <- function(v) ifelse(is.na(v), "n/a", sprintf("%.2f", v))

  p5 <- ggplot(sc_plot, aes(value, position, colour = metric, shape = metric,
                            group = factor(metric, levels = rev(metric_labels)))) +
    geom_vline(xintercept = 0, colour = ink2, linewidth = 0.3) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = metric_cols) +
    scale_shape_manual(values = c(16, 16, 17, 4)) +
    scale_x_continuous(limits = c(min(0, sc_plot$value), 1), breaks = seq(0, 1, 0.2)) +
    guides(colour = guide_legend(ncol = 1), shape = guide_legend(ncol = 1)) +
    labs(title = sprintf("GAR scorecard: %s", VERSION),
         subtitle = sprintf(paste0("Correlations for players with %d+ minutes\nHeld-out R-squared: attack %s, ",
                                   "defense %s\nYear-to-year r %.2f; team goal difference r %.2f"),
                            SCORE_MIN, fmt(mv[["heldout_r2_attack"]]), fmt(mv[["heldout_r2_defense"]]),
                            mv[["year_to_year_r"]], mv[["team_r_goal_diff"]]),
         x = "Correlation", y = NULL,
         caption = "Simulated player data; true talent is the simulation's answer key") +
    theme_gar + theme(panel.grid.major.y = element_blank())
  save(p5, "05_scorecard.png", h = 7)

  # 06: main score across versions (once there are at least two)
  if (n_distinct(history$version) >= 2) {
    hist_plot <- history %>%
      filter(metric == "r_talent_no_team") %>%
      mutate(position = factor(position, levels = c(pos_order, "Average")),
             # versions are shown in alphabetical order of their labels
             version  = factor(version, levels = sort(unique(history$version))))
    p6 <- ggplot(hist_plot, aes(position, value, colour = version, group = version)) +
      geom_line(linewidth = 0.6, alpha = 0.6) +
      geom_point(size = 2.8) +
      geom_hline(yintercept = 0, colour = ink2, linewidth = 0.3) +
      { if (n_distinct(hist_plot$version) <= 6)
          scale_colour_manual(values = c("#a8a6a0", "#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#7b5cd6")) } +
      scale_y_continuous(limits = c(min(0, hist_plot$value), 1)) +
      labs(title = "Is the method getting better?",
           subtitle = "GAR vs true talent with team strength removed, by version (higher is better)",
           x = NULL, y = "Correlation", caption = DATA_CAPTION) +
      theme_gar + theme(panel.grid.major.x = element_blank())
    save(p6, "06_scorecard_history.png")
  }
} else {
  cat("\nNo true_talent column: scorecard skipped (expected on real data).\n")
}

cat("\nDone. Outputs in", OUT_DIR, "\n")
