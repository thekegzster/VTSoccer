# =============================================================================
# prepare_real_data.R
#
# Turns the Wyscout exports (one per season) into the files player_gar.R and
# player_war.R read:
#
#   real_player_metrics.csv   one row per player per season
#   real_standings.csv        one row per team per season
#   real_team_name_map.csv    how each Wyscout team name was matched to the RPI file
#                             (check this once; fix any mistakes in TEAM_FIXES)
#
# Inputs (set the paths below):
#   Player_Stats_Conference_Adjusted.xlsx   the player stats (sheets Raw and Adjusted)
#   rpi_standings_all_fixed.csv             RPI rank and conference record
#   wyscout_complete_positions.xlsx         optional: market value and portal date
#
# What the real data is (and isn't):
#   - 2025 (full season, ~8-12 games per team; a few postseason teams up to ~18) and
#     2026 (to date, ~5-11), CONFERENCE GAMES ONLY (plus postseason in 2025)
#   - no player id, no class year: an id is built from name + team in the player's
#     first season, and a player keeps it in later seasons, also after a transfer
#     (matched on name plus birth country, height and age; see step 3b)
#   - no team results: goals for come from the players' goals, goals against from
#     the goalkeepers' goals conceded, games from team minutes / 990
#   - the "Adjusted" sheet already corrects each per-90 stat for conference strength,
#     so use it with ADJUST_SOS <- FALSE in the models (the default)
#   - not in this export: pressing duels, clearances, loose-ball duels, losses,
#     recoveries, missed balls, progressive passes allowed, team possession
#
# Run:  Rscript prepare_real_data.R      (from the folder with player_gar.R)
# Packages: readxl, dplyr, readr, stringr
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
})

# ---- Settings ---------------------------------------------------------------
# One export per season (same layout). Every season is written to the same output files.
STATS_FILES    <- c(`2025` = "Player_Stats_Conference_Adjusted_2025.xlsx",
                    `2026` = "Player_Stats_Conference_Adjusted.xlsx")
POSITIONS_FILE <- "wyscout_complete_positions.xlsx"   # optional ("" to skip); matched to the latest season
RPI_FILE       <- "rpi_standings_all_fixed.csv"
STAT_SHEET     <- "Adjusted"    # "Adjusted" (conference-adjusted per-90s) or "Raw"
OUT_PLAYERS    <- "real_player_metrics.csv"
OUT_STANDINGS  <- "real_standings.csv"
OUT_TEAM_MAP   <- "real_team_name_map.csv"
OUT_RAW_STATS  <- "real_player_stats_raw.csv"   # the Raw sheet as is, with player ids (for the app's Stats tab)
OUT_LINKS      <- "real_player_links.csv"       # players matched across seasons (check transfers here)
PEN_XG         <- 0.76          # xG of one penalty in Wyscout's model; used to remove penalties from xG

# Wyscout team names that don't start with the RPI name (Wyscout -> RPI)
TEAM_FIXES <- c(
  "Albany Great Danes"             = "UAlbany",
  "Berkeley Golden Bears"          = "California",
  "Boston Terriers"                = "Boston U.",
  "CCSU Blue Devils"               = "Central Connecticut",
  "Charleston Cougars"             = "College of Charleston",
  "FAU Owls"                       = "Florida Atlantic",
  "Fairleigh Dickinson Knights"    = "FDU",
  "IPFW Mastodons"                 = "Purdue Fort Wayne",
  "IU Indianapolis"                = "IU Indy",
  "JMU Dukes"                      = "James Madison",
  "Loyola Greyhounds"              = "Loyola Maryland",
  "NKU Norse"                      = "Northern Kentucky",
  "SFU Red Flash"                  = "Saint Francis",
  "SHU Pioneers"                   = "Sacred Heart",
  "St John's Red Storm"            = "St. John's",
  "St. Mary's College of CA Gaels" = "Saint Mary's",
  "UCONN Huskies"                  = "UConn",
  "UMASS Lowell River Hawks"       = "UMass Lowell",
  "UMKC Kangaroos"                 = "Kansas City",
  "USF Bulls"                      = "South Florida",
  "WIU Fighting Leathernecks"      = "Western Illinois"
)

# Column name the models use = column in the export.
# NA = not in the real export (the column is written empty).
STAT_MAP <- c(
  "Accelerations / 90"                  = "Accelerations per 90",
  "Aerial duels / 90 minutes"           = "Aerial duels per 90",
  "Aerial duels won (%)"                = "Aerial duels won, %",
  "Accurate long passes (%)"            = "Accurate long passes, %",
  "Accurate progressive passes (%)"     = "Accurate progressive passes, %",
  "Accurate smart passes (%)"           = "Accurate smart passes, %",
  "Accurate through passes (%)"         = "Accurate through passes, %",
  "Assists / 90"                        = "Assists per 90",
  "Clearances / 90"                     = NA,
  "Cross accuracy"                      = "Accurate crosses, %",
  "Cross accuracy from left flank (%)"  = "Accurate crosses from left flank, %",
  "Cross accuracy from right flank (%)" = "Accurate crosses from right flank, %",
  "Cross accuracy to six-yard box (%)"  = NA,
  "Crosses / 90"                        = "Crosses per 90",
  "Crosses from left flank, per 90"     = "Crosses from left flank per 90",
  "Crosses from right flank, per 90"    = "Crosses from right flank per 90",
  "Crosses to six-yard box, per 90 mins" = "Crosses to goalie box per 90",
  "Deep completed crosses / 90"         = "Deep completed crosses per 90",
  "Deep completions passes / 90"        = "Deep completions per 90",
  "Defensive duels / 90 minutes"        = "Defensive duels per 90",
  "Defensive duels won (%)"             = "Defensive duels won, %",
  "Dribbles / 90"                       = "Dribbles per 90",
  "Dribbles %"                          = "Successful dribbles, %",
  "Exits, per 90 mins"                  = "Exits per 90",
  "Fouls / 90"                          = "Fouls per 90",
  "Goal conversion (%)"                 = "Goal conversion, %",
  "Goals / 90"                          = "Goals per 90",
  "Head goals, avg per 90 mins"         = "Head goals per 90",
  "Interceptions / 90 minutes"          = "Interceptions per 90",
  # Wyscout's possession-adjusted versions stand in for the "per 30 minutes of
  # opponent possession" stats the models use
  "Interceptions / per 30 minutes of opponent possession"   = "PAdj Interceptions",
  "Sliding tackles / per 30 minutes of opponent possession" = "PAdj Sliding tackles",
  "Key passes / 90"                     = "Key passes per 90",
  "Long passes / 90 minutes"            = "Long passes per 90",
  "Loose ball duels / 90 minutes"       = NA,
  "Loss index"                          = NA,
  "Losses / 90"                         = NA,
  "Missed balls / 90 minutes"           = NA,
  "Non-penalty goals, per 90 mins"      = "Non-penalty goals per 90",
  "Offensive Duels / 90"                = "Offensive duels per 90",
  "Offensive Duels %"                   = "Offensive duels won, %",
  "Opponent half recoveries / 90"       = NA,
  "Own half losses / 90"                = NA,
  "Pass accuracy"                       = "Accurate passes, %",
  "Passes / 90"                         = "Passes per 90",
  "Passes to final third / 90"          = "Passes to final third per 90",
  "Passes to final third %"             = "Accurate passes to final third, %",
  "Passes to penalty area / 90"         = "Passes to penalty area per 90",
  "Passes to penalty area %"            = "Accurate passes to penalty area, %",
  "Pressing duels, per 90"              = NA,
  "Progressive passes / 90"             = "Progressive passes per 90",
  "Progressive passes allowed"          = NA,
  "Progressive runs / 90"               = "Progressive runs per 90",
  "Recoveries / 90"                     = NA,
  "Red cards / 90 minutes"              = "Red cards per 90",
  "Second assists / 90"                 = "Second assists per 90",
  "Short/medium passes / 90"            = "Short / medium passes per 90",
  "Short/medium passes %"               = "Accurate short / medium passes, %",
  "Shot assists / 90"                   = "Shot assists per 90",
  "Shots / 90"                          = "Shots per 90",
  "Shots against / 90"                  = "Shots against per 90",
  "Shots on target %"                   = "Shots on target, %",
  "Smart passes / 90"                   = "Smart passes per 90",
  "Successful attacking actions / 90"   = "Successful attacking actions per 90",
  "Successful defensive actions / 90"   = "Successful defensive actions per 90",
  "Third assists / 90"                  = "Third assists per 90",
  "Through passes / 90"                 = "Through passes per 90",
  "Touches in box, per 90"              = "Touches in box per 90",
  "xA / 90"                             = "xA per 90",
  "xG / 90"                             = "xG per 90",
  "Yellow cards / 90 minutes"           = "Yellow cards per 90",
  # more stats from the export, kept under these names
  "Shots blocked / 90"                  = "Shots blocked per 90",
  "Duels / 90"                          = "Duels per 90",
  "Duels won (%)"                       = "Duels won, %",
  "Received passes / 90"                = "Received passes per 90",
  "Received long passes / 90"           = "Received long passes per 90",
  "Fouls suffered / 90"                 = "Fouls suffered per 90",
  "Forward passes / 90"                 = "Forward passes per 90",
  "Accurate forward passes (%)"         = "Accurate forward passes, %",
  "Average pass length, m"              = "Average pass length, m",
  "Conceded goals / 90"                 = "Conceded goals per 90",
  "Save rate (%)"                       = "Save rate, %",
  "xG against / 90"                     = "xG against per 90",
  "Prevented goals / 90"                = "Prevented goals per 90"
)
# Season totals (from the Raw sheet; never adjusted)
TOTAL_MAP <- c(
  "Goals" = "Goals", "xG, Total" = "xG", "Shots, total" = "Shots",
  "Non-penalty goals, total" = "Non-penalty goals", "Head goals, total" = "Head goals",
  "Yellow cards, total" = "Yellow cards", "Red cards, total" = "Red cards",
  "Assists, total" = "Assists", "xA, total" = "xA",
  "Conceded goals, total" = "Conceded goals", "xG against, total" = "xG against",
  "Prevented goals, total" = "Prevented goals", "Clean sheets" = "Clean sheets",
  "Penalties taken" = "Penalties taken"
)

# The export has two "Aerial duels per 90" columns (the second is the goalkeeper one);
# keep the first name as is and call the second "Aerial duels per 90.1"
keep_names <- function(x) make.unique(x, sep = ".")
for (f in c(STATS_FILES, RPI_FILE)) if (!file.exists(f)) stop("Can't find ", f, ". Check the paths at the top.")
pick <- function(df, col) if (!is.na(col) && col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else NA_real_
num0 <- function(x) { x <- suppressWarnings(as.numeric(x)); if_else(!is.na(x) & x == 0, NA_real_, x) }
id_part <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "")
rpi_all <- read_csv(RPI_FILE, show_col_types = FALSE)

# Steps 1-4 for one season's export
prep_season <- function(SEASON, STATS_FILE) {
  cat(sprintf("\n== %d: %s\n", SEASON, STATS_FILE))

  # ---- 1. Read --------------------------------------------------------------
  raw   <- read_excel(STATS_FILE, sheet = "Raw", .name_repair = keep_names)
  stats <- read_excel(STATS_FILE, sheet = STAT_SHEET, .name_repair = keep_names)
  if (!identical(raw$Player, stats$Player) || !identical(raw$`Team (season)`, stats$`Team (season)`))
    stop("The Raw and ", STAT_SHEET, " sheets aren't in the same row order.")
  # a few rows have no usable position group (e.g. "Other" with 0 minutes)
  keep <- raw$`Position group` %in% c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")
  if (any(!keep)) cat(sprintf("Dropped %d row(s) without a position group\n", sum(!keep)))
  raw <- raw[keep, ]; stats <- stats[keep, ]

  missing_cols <- setdiff(na.omit(c(STAT_MAP, TOTAL_MAP)), c(names(stats), names(raw)))
  if (length(missing_cols)) warning("Not in the ", SEASON, " export (written empty): ", paste(missing_cols, collapse = ", "))

  # ---- 2. Team names: Wyscout -> RPI ----------------------------------------
  rpi <- rpi_all %>% filter(season_year == SEASON)
  if (!nrow(rpi)) warning("No ", SEASON, " rows in ", RPI_FILE, ": RPI rank and record will be empty.")
  rpi_names <- unique(rpi$team)
  match_team <- function(w) {
    if (w %in% names(TEAM_FIXES)) return(TEAM_FIXES[[w]])
    hits <- rpi_names[w == rpi_names | startsWith(w, paste0(rpi_names, " "))]
    if (length(hits)) hits[which.max(nchar(hits))] else NA_character_   # longest: "Virginia Tech" over "Virginia"
  }
  team_map <- tibble(season_year = SEASON, wyscout_team = sort(unique(raw$`Team (season)`))) %>%
    mutate(rpi_team = vapply(wyscout_team, match_team, character(1)),
           how = case_when(wyscout_team %in% names(TEAM_FIXES) ~ "TEAM_FIXES",
                           !is.na(rpi_team) ~ "automatic", TRUE ~ "NOT MATCHED"),
           team = coalesce(rpi_team, wyscout_team))
  n_miss <- sum(is.na(team_map$rpi_team))
  cat(sprintf("Teams: %d in the export, %d matched to %s, %d not matched (kept their Wyscout name)\n",
              nrow(team_map), nrow(team_map) - n_miss, RPI_FILE, n_miss))
  if (any(duplicated(team_map$rpi_team[!is.na(team_map$rpi_team)])))
    warning("Two Wyscout teams matched the same RPI team in ", SEASON, "; see ", OUT_TEAM_MAP)

  # ---- 3. Players -----------------------------------------------------------
  players <- tibble(
    season_year        = SEASON,
    conference         = raw$Conference,
    wyscout_team       = raw$`Team (season)`,
    conference_rating  = pick(raw, "Conference rating"),
    team_rating        = pick(raw, "Team rating"),          # Massey team rating
    team_possession_pct = 50,                                # not in the export: no possession adjustment
    player_name        = str_squish(raw$Player),
    position           = raw$`Position group`,
    position_detail    = raw$Position,
    class_year         = NA_character_,                      # not in the export
    age                = pick(raw, "Age"),
    height_cm          = num0(raw$Height),                   # 0 = unknown
    weight_kg          = num0(raw$Weight),
    foot               = na_if(raw$Foot, "unknown"),
    birth_country      = raw$`Birth country`,
    passport_country   = raw$`Passport country`,
    portal_status      = raw$`Transfer Portal Status`,
    matches_played     = pick(raw, "Matches played"),
    starts             = NA_real_,
    minutes_played     = pick(raw, "Minutes played")
  ) %>%
    left_join(team_map %>% select(wyscout_team, team), by = "wyscout_team")

  for (nm in names(STAT_MAP))  players[[nm]] <- pick(stats, STAT_MAP[[nm]])
  for (nm in names(TOTAL_MAP)) players[[nm]] <- pick(raw, TOTAL_MAP[[nm]])
  players[["xG + xA / 90"]] <- players[["xG / 90"]] + players[["xA / 90"]]

  # Non-penalty xG and shots. Penalties go to whoever the coach picks to take them, so they
  # say little about a player's own chance quality. Each penalty is worth about PEN_XG xG.
  pens   <- coalesce(players[["Penalties taken"]], 0)
  pen90  <- if_else(players$minutes_played > 0, pens * 90 / players$minutes_played, 0)
  players[["npxG, total"]]               <- pmax(players[["xG, Total"]] - PEN_XG * pens, 0)
  players[["npxG / 90"]]                 <- pmax(players[["xG / 90"]] - PEN_XG * pen90, 0)
  players[["Non-penalty shots / 90"]]    <- pmax(players[["Shots / 90"]] - pen90, 0)
  players[["npxG + xA / 90"]]            <- players[["npxG / 90"]] + players[["xA / 90"]]

  # GK-only stats are blank for outfield players and vice versa
  gk_only <- c("Exits, per 90 mins", "Shots against / 90", "Conceded goals / 90", "Save rate (%)",
               "xG against / 90", "Prevented goals / 90")
  for (nm in gk_only) players[[nm]][players$position != "GK"] <- NA_real_

  # id = team + name (the export has no player id); repeated names get -2, -3.
  # Step 3b swaps in a player's id from the season before when he can be matched.
  players <- players %>%
    mutate(player_id = make.unique(paste0(id_part(team), ":", id_part(player_name)), sep = "-"))

  # ---- 4. Teams (conference games in the export) ----------------------------
  team_tab <- players %>%
    group_by(season_year, team, conference) %>%
    summarise(wyscout_team = first(wyscout_team),
              team_rating = first(team_rating),
              conference_rating = first(conference_rating),
              team_minutes = sum(minutes_played, na.rm = TRUE),
              goals_for = sum(Goals, na.rm = TRUE),                          # own goals aren't credited to anyone
              goals_against = sum(`Conceded goals, total`[position == "GK"], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(games = round(team_minutes / 990, 2))                             # 11 players x 90 minutes

  standings <- team_tab %>%
    left_join(rpi %>% select(team, rpi_rank, conf_w, conf_l, conf_t, sheet_updated), by = "team") %>%
    mutate(
      # Record: the RPI file's conference record (may lag the Wyscout export by a game or two)
      overall_w = conf_w, overall_l = conf_l, overall_t = conf_t,
      win_pct = (conf_w + 0.5 * conf_t) / (conf_w + conf_l + conf_t),
      # Schedule: conference games only, so schedule strength = conference strength
      sos_rank = rank(-conference_rating, ties.method = "min")
    ) %>%
    select(season_year, team, conference, rpi_rank, sos_rank, overall_w, overall_l, overall_t,
           games, win_pct, goals_for, goals_against, team_rating, conference_rating,
           wyscout_team, record_as_of = sheet_updated)

  players <- players %>%
    left_join(standings %>% select(team, team_rpi_rank = rpi_rank, team_games = games), by = "team")

  # The Raw sheet as it is (real counts and per-90s, not conference-adjusted), for the app's Stats tab
  raw_out <- raw %>% mutate(season_year = SEASON, team = players$team, position = players$position)

  cat(sprintf("%d players, %d teams; games per team %.1f-%.1f; %d players with 270+ minutes\n",
              nrow(players), n_distinct(players$team), min(standings$games), max(standings$games),
              sum(players$minutes_played >= 270, na.rm = TRUE)))
  if (any(is.na(standings$win_pct)))
    cat("No RPI record for:", paste(standings$team[is.na(standings$win_pct)], collapse = ", "), "\n")
  list(players = players, standings = standings, raw = raw_out, team_map = team_map)
}

seasons <- lapply(sort(names(STATS_FILES)), function(y) prep_season(as.integer(y), STATS_FILES[[y]]))

# ---- 3b. Same player across seasons -------------------------------------------
# Season by season, a player takes the id he had the season before:
#   - same team and same name: same player
#   - otherwise (a transfer): same name at another team, if exactly one earlier player
#     fits AND something besides the name agrees: height (within 3 cm), birth country,
#     or he was in the transfer portal last season. Ages may differ by at most 1.
# Every match is listed in real_player_links.csv.
link_ids <- function(prev, cur) {
  prev_k <- prev %>% transmute(prev_id = player_id, player_name, team_prev = team, b_prev = birth_country,
                               h_prev = height_cm, a_prev = age, portal_prev = portal_status)
  stay <- cur %>% select(row, player_name, team) %>%
    inner_join(prev_k %>% select(prev_id, player_name, team = team_prev), by = c("player_name", "team"),
               relationship = "many-to-many") %>%
    group_by(row) %>% filter(n() == 1) %>% ungroup() %>%
    group_by(prev_id) %>% filter(n() == 1) %>% ungroup() %>%
    transmute(row, prev_id, how = "same team")
  moved <- cur %>% filter(!row %in% stay$row) %>%
    select(row, player_name, team, birth_country, height_cm, age) %>%
    inner_join(prev_k %>% filter(!prev_id %in% stay$prev_id), by = "player_name", relationship = "many-to-many") %>%
    filter(team != team_prev,
           is.na(age) | is.na(a_prev) | abs(age - a_prev) <= 1,
           is.na(birth_country) | is.na(b_prev) | birth_country == b_prev,
           is.na(height_cm) | is.na(h_prev) | abs(height_cm - h_prev) <= 3) %>%
    filter((!is.na(height_cm) & !is.na(h_prev)) | (!is.na(birth_country) & !is.na(b_prev)) | !is.na(portal_prev)) %>%
    group_by(row) %>% filter(n() == 1) %>% ungroup() %>%
    group_by(prev_id) %>% filter(n() == 1) %>% ungroup() %>%
    transmute(row, prev_id, how = "transfer")
  bind_rows(stay, moved)
}
link_log <- list()
for (i in seq_along(seasons)[-1]) {
  cur  <- seasons[[i]]$players %>% mutate(row = row_number())
  prev <- seasons[[i - 1]]$players
  ln <- link_ids(prev, cur)
  new_id <- cur$player_id
  new_id[ln$row] <- ln$prev_id
  # an unmatched player whose own id is already taken by a linked player gets the season added
  free <- setdiff(seq_along(new_id), ln$row)
  clash <- free[new_id[free] %in% new_id[ln$row]]
  new_id[clash] <- paste0(new_id[clash], "-", cur$season_year[1])
  stopifnot(!anyDuplicated(new_id))
  link_log[[i]] <- cur[ln$row, ] %>%
    transmute(season_year, player_name, team, position, minutes_played,
              earlier_season = prev$season_year[1], earlier_team = prev$team[match(ln$prev_id, prev$player_id)],
              earlier_position = prev$position[match(ln$prev_id, prev$player_id)],
              how = ln$how, player_id = ln$prev_id)
  seasons[[i]]$players$player_id <- new_id
  cat(sprintf("\n%d: %d players also played in %d (%d at the same team, %d transfers)\n", cur$season_year[1],
              nrow(ln), prev$season_year[1], sum(ln$how == "same team"), sum(ln$how == "transfer")))
}
for (i in seq_along(seasons)) seasons[[i]]$raw$player_id <- seasons[[i]]$players$player_id

players <- bind_rows(lapply(seasons, `[[`, "players"))

# Optional: market value and portal date from the positions workbook (latest season's teams)
if (nzchar(POSITIONS_FILE) && file.exists(POSITIONS_FILE)) {
  sheets <- setdiff(excel_sheets(POSITIONS_FILE),
                    c("Team Positions", "Position Detail", "Notes", "All Players"))
  extra <- bind_rows(lapply(sheets, function(s) {
    d <- read_excel(POSITIONS_FILE, sheet = s, .name_repair = keep_names)
    tibble(player_name = str_squish(d$Player),
           wyscout_team = d$`Team within selected timeframe`,
           market_value = suppressWarnings(as.numeric(d$`Market value`)),
           portal_initiated = as.character(as.Date(d$`Transfer Portal Initiated Date`)))
  })) %>%
    group_by(player_name, wyscout_team) %>%
    summarise(market_value = max(market_value, na.rm = TRUE) %>% replace(is.infinite(.), NA),
              portal_initiated = first(na.omit(portal_initiated)), .groups = "drop") %>%
    mutate(season_year = max(players$season_year))
  players <- left_join(players, extra, by = c("season_year", "player_name", "wyscout_team"))
}

players   <- players %>% relocate(season_year, conference, team, player_id, player_name, position)
standings <- bind_rows(lapply(seasons, `[[`, "standings"))
raw_out   <- bind_rows(lapply(seasons, `[[`, "raw")) %>% relocate(season_year, team, player_id, position)

write_csv(players, OUT_PLAYERS, na = "")
write_csv(raw_out, OUT_RAW_STATS, na = "")
write_csv(standings, OUT_STANDINGS, na = "")
write_csv(bind_rows(lapply(seasons, `[[`, "team_map")), OUT_TEAM_MAP)
write_csv(bind_rows(link_log), OUT_LINKS, na = "")

cat(sprintf("\nWrote %s: %d player-seasons (%s), %s sheet\n", OUT_PLAYERS, nrow(players),
            paste(sprintf("%s: %d", names(table(players$season_year)), as.integer(table(players$season_year))), collapse = ", "),
            STAT_SHEET))
cat(sprintf("Wrote %s, %s, %s and %s\n", OUT_STANDINGS, OUT_RAW_STATS, OUT_TEAM_MAP, OUT_LINKS))
cat("Players by position and season:\n"); print(table(players$position, players$season_year))
