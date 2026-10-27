# =============================================================================
# Load the public Wyscout event dataset (Pappalardo et al., 2019) in R
#
# 2017-18 season: England, France, Germany, Italy, Spain, plus Euro 2016 and
# World Cup 2018. Same files and download links as the Python notebook.
#
# Produces these data frames:
#   competitions   one row per competition
#   teams          one row per team
#   players        one row per player (names cleaned, role/foot/height etc.)
#   matches        one row per match
#   match_teams    one row per team per match (side, score, formation)
#   lineups        one row per player per match (starter/bench, goals, cards)
#   substitutions  one row per substitution
#   events         one row per event, with start/end coordinates, tags, and
#                  player and team names joined on
#
# Pitch coordinates are 0-100 (% of pitch length/width), from the point of
# view of the team making the action (attacking left to right).
# =============================================================================

# install.packages(c("jsonlite", "dplyr", "tidyr", "purrr", "stringi", "readr"))
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringi)

# ---- Settings ---------------------------------------------------------------

DATA_DIR    <- "example_data/wyscout"
COMPETITION <- "England"   # file suffix: England, France, Germany, Italy, Spain,
# European_Championship, World_Cup

# Storage savers (this is test data, so a slice of the season is plenty)
SAMPLE_MATCHES   <- 100    # keep only the first N matches of the season (by date); NULL = all
COMPRESS_CSV     <- TRUE   # save CSVs gzipped (.csv.gz), several times smaller; pandas and
# readr read them directly
DELETE_DOWNLOADS <- FALSE  # TRUE = delete the two zip files after the CSVs are saved (frees the
# most space, but the next run of this script downloads them again)

# ---- 1. Download (only what's missing) -------------------------------------
# The zip files are read directly below, so nothing is unzipped to disk.

files <- c(
  "competitions.json" = "https://ndownloader.figshare.com/files/15073685",
  "teams.json"        = "https://ndownloader.figshare.com/files/15073697",
  "players.json"      = "https://ndownloader.figshare.com/files/15073721",
  "matches.zip"       = "https://ndownloader.figshare.com/files/14464622",
  "events.zip"        = "https://ndownloader.figshare.com/files/14464685"
)

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
options(timeout = 3600)   # the events file is large; R's default 60-second limit is too short

for (name in names(files)) {
  path <- file.path(DATA_DIR, name)
  if (!file.exists(path)) {
    message("Downloading ", name)
    download.file(files[[name]], path, mode = "wb")   # "wb" is required on Windows for zip files
  }
}

matches_zip  <- file.path(DATA_DIR, "matches.zip")
events_zip   <- file.path(DATA_DIR, "events.zip")
matches_name <- paste0("matches_", COMPETITION, ".json")
events_name  <- paste0("events_",  COMPETITION, ".json")
in_zip <- unzip(events_zip, list = TRUE)$Name
if (!events_name %in% in_zip) stop("No events file for '", COMPETITION, "'. In the zip: ",
                                   paste(in_zip, collapse = ", "))

# Earlier versions of this script unzipped every competition here. Those files
# aren't needed any more, so remove them to free the space.
old_unzipped <- list.files(DATA_DIR, pattern = "^(events|matches)_.*\\.json$", full.names = TRUE)
if (length(old_unzipped)) {
  message("Removing ", length(old_unzipped), " previously unzipped files to free space")
  file.remove(old_unzipped)
}

# Some names in this dataset are stored with escaped accents (e.g. "Al\\u00e9").
# This turns them back into normal characters; plain names are left unchanged.
fix_text <- function(df) {
  mutate(df, across(where(is.character), ~ ifelse(grepl("\\\\u[0-9a-fA-F]{4}", .x),
                                                  stri_unescape_unicode(.x), .x)))
}

# ---- 2. Competitions, teams, players ----------------------------------------

competitions <- fromJSON(file.path(DATA_DIR, "competitions.json"), flatten = TRUE) %>%
  as_tibble() %>% fix_text()

teams <- fromJSON(file.path(DATA_DIR, "teams.json"), flatten = TRUE) %>%
  as_tibble() %>% fix_text() %>%
  rename(team_id = wyId, team_name = name)

players <- fromJSON(file.path(DATA_DIR, "players.json"), flatten = TRUE) %>%
  as_tibble() %>% fix_text() %>%
  rename(player_id = wyId, player_name = shortName) %>%
  mutate(across(any_of(c("currentTeamId", "currentNationalTeamId")), ~ suppressWarnings(as.integer(.x))))

# ---- 3. Matches, teams in each match, lineups, substitutions ----------------
# Each match stores its two teams under their team id, so it's read as lists
# and turned into tidy tables.

matches_raw <- fromJSON(unz(matches_zip, matches_name), simplifyVector = FALSE)

# Keep only the first SAMPLE_MATCHES matches of the season
if (!is.null(SAMPLE_MATCHES)) {
  kickoff <- vapply(matches_raw, function(m) m$dateutc, "")
  matches_raw <- matches_raw[order(kickoff)][seq_len(min(SAMPLE_MATCHES, length(matches_raw)))]
  message("Keeping the first ", length(matches_raw), " matches of the season")
}
keep_match_ids <- vapply(matches_raw, function(m) m$wyId, numeric(1))

matches <- map_dfr(matches_raw, function(m) tibble(
  match_id       = m$wyId,
  label          = m$label,
  date_utc       = as.POSIXct(m$dateutc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  competition_id = m$competitionId,
  season_id      = m$seasonId,
  gameweek       = m$gameweek %||% NA_integer_,
  winner         = m$winner %||% NA_integer_,   # team id; 0 = draw
  venue          = m$venue %||% NA_character_,
  duration       = m$duration %||% NA_character_
)) %>% fix_text()

match_teams <- map_dfr(matches_raw, function(m) map_dfr(m$teamsData, function(t) tibble(
  match_id         = m$wyId,
  team_id          = t$teamId,
  side             = t$side,
  score            = t$score,
  score_ht         = t$scoreHT %||% NA_integer_,
  has_formation    = t$hasFormation %||% NA_integer_
))) %>%
  group_by(match_id) %>%
  mutate(opponent_id = rev(team_id), goals_against = rev(score)) %>%
  ungroup()

lineups <- map_dfr(matches_raw, function(m) map_dfr(m$teamsData, function(t) {
  f <- t$formation
  one <- function(lst, role) {
    if (is.null(lst) || length(lst) == 0) return(NULL)
    map_dfr(lst, ~ tibble(
      match_id    = m$wyId,
      team_id     = t$teamId,
      player_id   = .x$playerId,
      role        = role,
      goals       = suppressWarnings(as.integer(.x$goals %||% NA)),
      own_goals   = suppressWarnings(as.integer(.x$ownGoals %||% NA)),
      # in this dataset the card fields hold the MINUTE of the card, 0 = no card
      yellow_minute = suppressWarnings(as.integer(.x$yellowCards %||% NA)),
      red_minute    = suppressWarnings(as.integer(.x$redCards %||% NA))
    ))
  }
  bind_rows(one(f$lineup, "starter"), one(f$bench, "bench"))
}))

substitutions <- map_dfr(matches_raw, function(m) map_dfr(m$teamsData, function(t) {
  subs <- t$formation$substitutions
  if (is.null(subs) || length(subs) == 0 || identical(subs, "null")) return(NULL)
  map_dfr(subs, ~ tibble(match_id = m$wyId, team_id = t$teamId,
                         player_in = .x$playerIn, player_out = .x$playerOut,
                         minute = .x$minute))
}))

rm(matches_raw)

# ---- 4. Events --------------------------------------------------------------
# Each event has a list of positions (start, and usually end) and a list of tags
# (e.g. 101 = goal, 1801 = accurate, 1802 = not accurate).

message("Reading events (this can take a minute or two)")
ev <- fromJSON(unz(events_zip, events_name))
ev <- ev[ev$matchId %in% keep_match_ids, ]   # only events from the kept matches

first_or_na <- function(p, i, col) if (NROW(p) >= i) p[[col]][i] else NA_real_
pos <- ev$positions
tag_ids <- vapply(ev$tags, function(t) if (NROW(t)) paste(t$id, collapse = ",") else "", "")
has_tag <- function(id) grepl(paste0("(^|,)", id, "(,|$)"), tag_ids)

events <- tibble(
  event_id      = ev$id,
  match_id      = ev$matchId,
  team_id       = ev$teamId,
  player_id     = ev$playerId,          # 0 = no player (e.g. some ball-out events)
  match_period  = ev$matchPeriod,       # 1H, 2H, E1, E2, P
  event_sec     = ev$eventSec,          # seconds since the start of the period
  event_type_id = ev$eventId,
  event_name    = ev$eventName,         # Pass, Shot, Duel, Foul, ...
  sub_event_id  = suppressWarnings(as.integer(ev$subEventId)),
  sub_event_name= ev$subEventName,      # Simple pass, Cross, Ground attacking duel, ...
  start_x       = vapply(pos, first_or_na, numeric(1), i = 1, col = "x"),
  start_y       = vapply(pos, first_or_na, numeric(1), i = 1, col = "y"),
  end_x         = vapply(pos, first_or_na, numeric(1), i = 2, col = "x"),
  end_y         = vapply(pos, first_or_na, numeric(1), i = 2, col = "y"),
  tags          = tag_ids,              # all tag ids, comma-separated
  is_goal       = has_tag(101),
  is_accurate   = has_tag(1801),
  is_assist     = has_tag(301),
  is_key_pass   = has_tag(302)
) %>%
  left_join(players %>% select(player_id, player_name), by = "player_id") %>%
  left_join(teams %>% select(team_id, team_name), by = "team_id") %>%
  relocate(player_name, team_name, .after = player_id)

rm(ev, pos, tag_ids)

# ---- 5. Save as CSV (for Python, e.g. radar_plots.py) ------------------------
# Written to data/wyscout/tidy/<COMPETITION>/. Set SAVE_CSV <- FALSE to skip.

SAVE_CSV <- TRUE
if (SAVE_CSV) {
  out_dir <- file.path(DATA_DIR, "tidy", COMPETITION)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ext   <- if (COMPRESS_CSV) ".csv.gz" else ".csv"
  other <- if (COMPRESS_CSV) ".csv" else ".csv.gz"
  # only keep players and teams that appear in the saved matches
  teams   <- filter(teams, team_id %in% match_teams$team_id)
  players <- filter(players, player_id %in% c(lineups$player_id, events$player_id))
  for (nm in c("competitions", "teams", "players", "matches", "match_teams",
               "lineups", "substitutions", "events")) {
    readr::write_csv(get(nm), file.path(out_dir, paste0(nm, ext)), na = "")
    unlink(file.path(out_dir, paste0(nm, other)))   # remove an older copy in the other format
  }
  message("Saved CSV files to ", out_dir)
}

if (DELETE_DOWNLOADS) {
  file.remove(c(matches_zip, events_zip))
  message("Deleted the downloaded zip files (they'll be downloaded again on the next run)")
}

# ---- 6. Quick summary -------------------------------------------------------

message(sprintf("%s: %d matches, %d events, %d players in lineups",
                COMPETITION, nrow(matches), nrow(events), n_distinct(lineups$player_id)))
print(count(events, event_name, sort = TRUE))