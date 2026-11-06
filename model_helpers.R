# =============================================================================
# model_helpers.R: shared by player_gar.R and player_war.R (they source() it)
#
#   shrink_stats()  small-sample blending, separately for every stat
#   team_folds()    cross-validation groups that keep a team's seasons together
# =============================================================================

# Win-% stats: the per-90 column that counts the attempts behind each percentage.
# A 5-for-6 record says less than 50-for-60, so these stats are blended by attempts.
ATTEMPT_COLS <- c(def_duel_win_pct = "Defensive duels / 90 minutes",
                  aerial_win_pct   = "Aerial duels / 90 minutes",
                  pass_acc         = "Passes / 90",
                  long_pass_acc    = "Long passes / 90 minutes",
                  off_duel_win_pct = "Offensive Duels / 90",
                  duel_win_pct     = "Duels / 90",
                  save_pct         = "Shots against / 90")

# Columns shrink_stats() needs next to each stat value, from the players table
stat_context <- function(players) {
  z <- function(x) dplyr::coalesce(as.numeric(x), 0)
  col <- function(nm) if (nm %in% names(players)) z(players[[nm]]) else rep(0, nrow(players))
  out <- tibble::tibble(player_id = players$player_id, season_year = players$season_year,
                        shots90 = col("Shots / 90"), sa90 = col("Shot assists / 90"),
                        xga90 = col("xG against / 90"))
  for (st in names(ATTEMPT_COLS)) out[[paste0("att_", st)]] <- col(ATTEMPT_COLS[[st]])
  out
}

# Small-sample blending, one stat at a time (empirical Bayes).
#
# d: one row per player x stat with season_year, position, stat, raw (per 90 or %),
#    minutes_played, qualified, joined to stat_context() by player_id and season_year.
#
# For each player and stat:
#   noise   v = c x 90 / minutes, his number's sampling noise:
#             counts per 90          c = the position's average rate
#             percentages            c = p(100 - p) / attempts per 90 (his own attempts)
#             xG, xA                 c = 2 x rate^2 / shots (or shot assists) per 90
#             goals prevented (GK)   c = xG faced per 90
#   spread  T = how much players at the position really differ on the stat:
#             the spread of qualified players' numbers minus their average noise
#             (both seasons pooled, each season centered on its own average)
#   blended value = position average + T / (T + v) x (his number - position average)
# So a stat that settles fast (pass accuracy) keeps most of a player's own number,
# and a noisy one (xG, xA) is pulled much harder toward average. prior_minutes = c x
# 90 / T is the same thing in the old units: how many minutes of average play he's
# treated as having on top of his own. post_var is the noise left after blending.
#
# mode "minutes" = the old way: every stat gets the same prior_min minutes
# (fixed_minutes overrides that for named stats, e.g. goals prevented).
shrink_stats <- function(d, mode = "per_stat", prior_min = 270, fixed_minutes = c(),
                         min_true_share = 0.05) {
  att <- rep(NA_real_, nrow(d))
  for (st in names(ATTEMPT_COLS)) {
    i <- d$stat == st
    if (any(i)) att[i] <- d[[paste0("att_", st)]][i]
  }
  d$att <- att
  d %>%
    dplyr::group_by(season_year, position, stat) %>%
    dplyr::mutate(missing   = is.na(raw),
                  prior     = mean(raw[qualified], na.rm = TRUE),
                  raw       = dplyr::coalesce(raw, prior),
                  shots_bar = mean(shots90[qualified], na.rm = TRUE),
                  sa_bar    = mean(sa90[qualified], na.rm = TRUE),
                  xga_bar   = mean(xga90[qualified], na.rm = TRUE),
                  c = dplyr::case_when(
                    !is.na(att)              ~ pmax(prior, 1) * pmax(100 - prior, 1) / pmax(att, 0.5),
                    stat == "xg"             ~ 2 * pmax(prior, 0)^2 / pmax(shots_bar, 0.1),
                    stat == "xa"             ~ 2 * pmax(prior, 0)^2 / pmax(sa_bar, 0.1),
                    stat == "prevented_goals" ~ pmax(xga_bar, 0.1),
                    TRUE                     ~ pmax(abs(prior), 1e-6)),
                  v   = c * 90 / pmax(minutes_played, 1),
                  dev = raw - prior) %>%
    dplyr::group_by(position, stat) %>%
    dplyr::mutate(true_var = {
      q <- qualified & !missing
      tv <- stats::var(dev[q]) - mean(v[q])
      max(tv, min_true_share * stats::var(dev[q]), 1e-12)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pm = if (mode == "per_stat") c * 90 / true_var else
             dplyr::coalesce(unname(fixed_minutes[stat]), prior_min),
      own_share = minutes_played / (minutes_played + pm),
      value     = prior + own_share * dev,
      post_var  = c * 90 / (pmax(minutes_played, 1) + pm)) %>%
    dplyr::select(-att, -shots_bar, -sa_bar, -xga_bar, -dev, -shots90, -sa90, -xga90,
                  -dplyr::starts_with("att_"))
}

# Cross-validation groups. With 3+ seasons, one season per group; otherwise 10 groups
# of TEAMS, so a team's two seasons are never split between fitting and testing
# (they'd share players and style, which makes the held-out score look better than it is).
team_folds <- function(fit_rows, k = 10) {
  if (dplyr::n_distinct(fit_rows$season_year) >= 3) return(as.integer(factor(fit_rows$season_year)))
  teams <- unique(fit_rows$team)
  fold_of <- stats::setNames(sample(rep_len(seq_len(k), length(teams))), teams)
  unname(fold_of[fit_rows$team])
}
