# =============================================================================
# Portal Recruiting Board  (R Shiny)
#
# Search every player, filter the list, and open a profile with:
#   - GAR, GAR per 90, 80% ranges, ranks, wins
#   - where the GAR comes from (attack / defense / replacement)
#   - how he compares with our weakest regular at his position
#   - GAR per 90 for every season he played
#   - every Wyscout skill: per-90 value + percentile at his position
#
# It reads the files player_gar.R writes, so re-run player_gar.R and restart
# the app to pick up new ratings.
#
# Run it from R:
#   shiny::runApp("recruiting_app")          # from the folder holding player_gar.R
# or from a terminal:
#   Rscript -e 'shiny::runApp("recruiting_app", launch.browser = TRUE)'
#
# Packages: shiny, bslib, DT, dplyr, tidyr, readr, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- Settings ----------------------------------------------------------------
# Relative paths start from this app's folder. If a file isn't there, the app
# searches the nearby folders for it. You can also give full paths with forward
# slashes, e.g. "C:/Users/you/Documents/project/GAR_output_real".
GAR_DIR       <- "../GAR_output_real"                 # output folder of player_gar.R
PAR_DIR       <- "../par_output_real"                 # output folder of player_war.R (OUTCOME "points")
METRICS_FILE  <- "../real_player_metrics.csv"         # the Wyscout player file (prepare_real_data.R)
STATS_FILE    <- "../real_player_stats_raw.csv"       # Season stats tab: real counts, not conference-adjusted
FOCUS_TEAM    <- "Virginia Tech"
ROSTER_SEASON <- 2026      # season player_gar.R compares against (ROSTER_SEASON there)
PROJ_MINUTES  <- 1500      # season length behind "+Goals" (PROJ_MINUTES there)
COMPARE_MIN   <- 360       # minutes needed for the roster comparison (COMPARE_MIN there)
PCT_MIN       <- 270       # minutes needed to be in the percentile comparison group
UPGRADE_MIN   <- 0.70      # "likely upgrade" filter: chance better than our weakest regular
GOALS_PER_WIN <- round(1 / 0.23, 1)

MAROON <- "#861F41"
ORANGE <- "#E5751F"
GREY   <- "#B9B3AE"
SLATE  <- "#44546A"
# Player colors in comparisons: Virginia Tech players in VT orange, everyone else in blue.
# When both players are on the same side, the comparison player gets a lighter shade.
VT_ORANGE  <- "#E5751F"; VT_ORANGE2 <- "#F4B886"
BLUE       <- "#2F6DB5"; BLUE2      <- "#9CBFE6"
DEFENSE_COL <- "#D9A3B4"   # GAR's defense part (light maroon, paired with the maroon attack part)

# ---- Skill list: id, Wyscout column, label, group, lower is better ----------
SKILLS <- tribble(
  ~id, ~col, ~label, ~group, ~lower,
  "goals_90", "Goals / 90", "Goals per 90", "Shooting", FALSE,
  "npg_90", "Non-penalty goals, per 90 mins", "Non-penalty goals per 90", "Shooting", FALSE,
  "xg_90", "xG / 90", "Expected goals (xG) per 90", "Shooting", FALSE,
  "npxg_90", "npxG / 90", "Non-penalty xG per 90", "Shooting", FALSE,
  "shots_90", "Shots / 90", "Shots per 90", "Shooting", FALSE,
  "sot_pct", "Shots on target %", "Shots on target %", "Shooting", FALSE,
  "conversion_pct", "Goal conversion (%)", "Goal conversion %", "Shooting", FALSE,
  "head_goals_90", "Head goals, avg per 90 mins", "Headed goals per 90", "Shooting", FALSE,
  "box_touches_90", "Touches in box, per 90", "Touches in the box per 90", "Shooting", FALSE,
  "xa_90", "xA / 90", "Expected assists (xA) per 90", "Chance creation", FALSE,
  "assists_90", "Assists / 90", "Assists per 90", "Chance creation", FALSE,
  "shot_assists_90", "Shot assists / 90", "Shot assists per 90", "Chance creation", FALSE,
  "key_passes_90", "Key passes / 90", "Key passes per 90", "Chance creation", FALSE,
  "second_assists_90", "Second assists / 90", "Second assists per 90", "Chance creation", FALSE,
  "smart_passes_90", "Smart passes / 90", "Smart passes per 90", "Chance creation", FALSE,
  "smart_pass_pct", "Accurate smart passes (%)", "Smart pass accuracy %", "Chance creation", FALSE,
  "through_passes_90", "Through passes / 90", "Through passes per 90", "Chance creation", FALSE,
  "through_pass_pct", "Accurate through passes (%)", "Through pass accuracy %", "Chance creation", FALSE,
  "deep_completions_90", "Deep completions passes / 90", "Deep completions per 90", "Chance creation", FALSE,
  "box_passes_90", "Passes to penalty area / 90", "Passes into the box per 90", "Chance creation", FALSE,
  "box_pass_pct", "Passes to penalty area %", "Pass-into-box accuracy %", "Chance creation", FALSE,
  "passes_90", "Passes / 90", "Passes per 90", "Passing", FALSE,
  "pass_pct", "Pass accuracy", "Pass accuracy %", "Passing", FALSE,
  "short_pass_pct", "Short/medium passes %", "Short/medium pass accuracy %", "Passing", FALSE,
  "long_passes_90", "Long passes / 90 minutes", "Long passes per 90", "Passing", FALSE,
  "long_pass_pct", "Accurate long passes (%)", "Long pass accuracy %", "Passing", FALSE,
  "prog_passes_90", "Progressive passes / 90", "Progressive passes per 90", "Passing", FALSE,
  "prog_pass_pct", "Accurate progressive passes (%)", "Progressive pass accuracy %", "Passing", FALSE,
  "final_third_passes_90", "Passes to final third / 90", "Passes into final third per 90", "Passing", FALSE,
  "final_third_pass_pct", "Passes to final third %", "Final-third pass accuracy %", "Passing", FALSE,
  "crosses_90", "Crosses / 90", "Crosses per 90", "Crossing", FALSE,
  "cross_pct", "Cross accuracy", "Cross accuracy %", "Crossing", FALSE,
  "deep_crosses_90", "Deep completed crosses / 90", "Deep completed crosses per 90", "Crossing", FALSE,
  "dribbles_90", "Dribbles / 90", "Dribbles per 90", "Carrying and 1v1", FALSE,
  "dribble_pct", "Dribbles %", "Dribble success %", "Carrying and 1v1", FALSE,
  "prog_runs_90", "Progressive runs / 90", "Progressive runs per 90", "Carrying and 1v1", FALSE,
  "accelerations_90", "Accelerations / 90", "Accelerations per 90", "Carrying and 1v1", FALSE,
  "off_duels_90", "Offensive Duels / 90", "Offensive duels per 90", "Carrying and 1v1", FALSE,
  "off_duel_pct", "Offensive Duels %", "Offensive duels won %", "Carrying and 1v1", FALSE,
  "att_actions_90", "Successful attacking actions / 90", "Successful attacking actions per 90", "Carrying and 1v1", FALSE,
  "def_duels_90", "Defensive duels / 90 minutes", "Defensive duels per 90", "Defending", FALSE,
  "def_duel_pct", "Defensive duels won (%)", "Defensive duels won %", "Defending", FALSE,
  "interceptions_30", "Interceptions / per 30 minutes of opponent possession", "Interceptions per 30 opp. poss. min", "Defending", FALSE,
  "sliding_30", "Sliding tackles / per 30 minutes of opponent possession", "Sliding tackles per 30 opp. poss. min", "Defending", FALSE,
  "def_actions_90", "Successful defensive actions / 90", "Successful defensive actions per 90", "Defending", FALSE,
  "recoveries_90", "Recoveries / 90", "Recoveries per 90", "Defending", FALSE,
  "opp_half_recoveries_90", "Opponent half recoveries / 90", "Opponent-half recoveries per 90", "Defending", FALSE,
  "pressing_90", "Pressing duels, per 90", "Pressing duels per 90", "Defending", FALSE,
  "loose_duels_90", "Loose ball duels / 90 minutes", "Loose-ball duels per 90", "Defending", FALSE,
  "clearances_90", "Clearances / 90", "Clearances per 90", "Defending", FALSE,
  "prog_allowed_90", "Progressive passes allowed", "Prog. passes allowed (fewer better)", "Defending", TRUE,
  "aerials_90", "Aerial duels / 90 minutes", "Aerial duels per 90", "Aerial", FALSE,
  "aerial_pct", "Aerial duels won (%)", "Aerial duels won %", "Aerial", FALSE,
  "losses_90", "Losses / 90", "Ball losses per 90", "Security (fewer better)", TRUE,
  "own_half_losses_90", "Own half losses / 90", "Own-half losses per 90", "Security (fewer better)", TRUE,
  "missed_balls_90", "Missed balls / 90 minutes", "Missed balls per 90", "Security (fewer better)", TRUE,
  "fouls_90", "Fouls / 90", "Fouls per 90", "Security (fewer better)", TRUE,
  "yellow_90", "Yellow cards / 90 minutes", "Yellow cards per 90", "Security (fewer better)", TRUE,
  "red_90", "Red cards / 90 minutes", "Red cards per 90", "Security (fewer better)", TRUE,
  "shots_against_90", "Shots against / 90", "Shots faced per 90 (fewer better)", "Goalkeeping", TRUE,
  "exits_90", "Exits, per 90 mins", "Exits per 90", "Goalkeeping", FALSE,
  # in the real Wyscout export only (dropped automatically when a file lacks them)
  "shots_blocked_90", "Shots blocked / 90", "Shots blocked per 90", "Defending", FALSE,
  "prevented_90", "Prevented goals / 90", "Goals prevented per 90 (vs xG faced)", "Goalkeeping", FALSE,
  "save_pct", "Save rate (%)", "Save rate %", "Goalkeeping", FALSE,
  "conceded_90", "Conceded goals / 90", "Goals conceded per 90 (fewer better)", "Goalkeeping", TRUE
)
GROUP_ORDER <- unique(SKILLS$group)

# The handful of skills that matter most at each position (radar and "key skills" view).
# Any that a data file doesn't have are skipped.
KEY_SKILLS <- list(
  GK = c("prevented_90", "save_pct", "conceded_90", "exits_90", "pass_pct", "long_pass_pct", "shots_against_90"),
  CB = c("def_duel_pct", "aerial_pct", "interceptions_30", "shots_blocked_90", "def_actions_90",
         "pass_pct", "prog_passes_90", "long_pass_pct"),
  FB = c("def_duel_pct", "interceptions_30", "def_actions_90", "crosses_90", "xa_90",
         "prog_runs_90", "prog_passes_90", "dribbles_90"),
  DM = c("interceptions_30", "def_duel_pct", "aerial_pct", "def_actions_90", "passes_90",
         "pass_pct", "prog_passes_90", "final_third_passes_90"),
  CM = c("passes_90", "pass_pct", "prog_passes_90", "key_passes_90", "xa_90", "xg_90",
         "interceptions_30", "def_duel_pct"),
  AM = c("xg_90", "xa_90", "key_passes_90", "shot_assists_90", "through_passes_90",
         "dribbles_90", "box_touches_90", "prog_runs_90"),
  W  = c("xg_90", "xa_90", "shot_assists_90", "dribbles_90", "dribble_pct", "crosses_90",
         "box_touches_90", "prog_runs_90"),
  CF = c("xg_90", "npg_90", "shots_90", "conversion_pct", "box_touches_90", "xa_90",
         "aerial_pct", "off_duel_pct")
)
GK_GROUPS   <- c("Passing", "Aerial", "Security (fewer better)", "Goalkeeping")

POS_LABELS <- c(GK = "Goalkeeper", CB = "Center back", FB = "Fullback",
                DM = "Defensive mid", CM = "Central mid", AM = "Attacking mid",
                W = "Winger", CF = "Center forward")
CLASS_LABELS <- c(Fr = "Freshman", So = "Sophomore", Jr = "Junior", Sr = "Senior", Gr = "Graduate")

# Country name (as Wyscout writes it) -> 3-letter code of its flag in www/CountryFlags.
# Drop a new <CODE>.png in that folder and the header picks it up; a country without a
# flag file shows its code instead. England, Scotland, Wales and Northern Ireland use
# GBR.png unless their own file is there.
FLAG_DIR <- "CountryFlags"
COUNTRY_CODES <- c(
  "United States" = "USA", "England" = "ENG", "Scotland" = "SCO", "Wales" = "WAL",
  "Northern Ireland" = "NIR", "United Kingdom" = "GBR", "Great Britain" = "GBR",
  "Canada" = "CAN", "Spain" = "ESP", "Germany" = "GER", "Norway" = "NOR", "France" = "FRA",
  "Denmark" = "DEN", "Brazil" = "BRA", "Italy" = "ITA", "Japan" = "JPN", "Ghana" = "GHA",
  "Australia" = "AUS", "Portugal" = "POR", "Sweden" = "SWE", "Iceland" = "ISL", "Israel" = "ISR",
  "Netherlands" = "NED", "Costa Rica" = "CRC", "Argentina" = "ARG", "Republic of Ireland" = "IRL",
  "Jamaica" = "JAM", "Austria" = "AUT", "Belgium" = "BEL", "Mexico" = "MEX", "Cyprus" = "CYP",
  "Uganda" = "UGA", "Serbia" = "SRB", "New Zealand" = "NZL", "Senegal" = "SEN", "Colombia" = "COL",
  "Puerto Rico" = "PUR", "South Africa" = "RSA", "Switzerland" = "SUI", "Chile" = "CHI",
  "Finland" = "FIN", "Slovenia" = "SLO", "Bermuda" = "BER", "Czech Republic" = "CZE",
  "Georgia" = "GEO", "Haiti" = "HAI", "Bolivia" = "BOL", "Ecuador" = "ECU", "Greece" = "GRE",
  "Hungary" = "HUN", "Kenya" = "KEN", "Korea Republic" = "KOR", "Trinidad and Tobago" = "TTO",
  "Venezuela" = "VEN", "Guatemala" = "GUA", "Latvia" = "LAT", "Lithuania" = "LTU", "Nigeria" = "NGR",
  "Poland" = "POL", "Bahamas" = "BAH", "Bangladesh" = "BAN", "Cameroon" = "CMR",
  "Cayman Islands" = "CAY", "China PR" = "CHN", "Egypt" = "EGY", "Faroe Islands" = "FRO",
  "Luxembourg" = "LUX", "Morocco" = "MAR", "Panama" = "PAN", "Romania" = "ROU", "Russia" = "RUS",
  "United Arab Emirates" = "UAE", "Zambia" = "ZAM", "Andorra" = "AND", "Angola" = "ANG",
  "Armenia" = "ARM", "Bosnia and Herzegovina" = "BIH", "Bulgaria" = "BUL", "Burkina Faso" = "BUR",
  "Chinese Taipei" = "TPE", "Congo DR" = "COD", "Congo" = "CGO", "Cuba" = "CUB",
  "Dominican Republic" = "DOM", "El Salvador" = "ESA", "Estonia" = "EST", "Ethiopia" = "ETH",
  "Grenada" = "GRN", "Honduras" = "HON", "India" = "IND", "Jordan" = "JOR", "Korea DPR" = "PRK",
  "Liberia" = "LBR", "Mali" = "MLI", "Montenegro" = "MNE", "North Macedonia" = "MKD", "Peru" = "PER",
  "Philippines" = "PHI", "Singapore" = "SGP", "Slovakia" = "SVK", "Suriname" = "SUR",
  "Ukraine" = "UKR", "US Virgin Islands" = "ISV", "Uruguay" = "URU", "Zimbabwe" = "ZIM",
  "Croatia" = "CRO", "Kazakhstan" = "KAZ", "Mongolia" = "MGL", "Moldova" = "MDA",
  "Thailand" = "THA", "Uzbekistan" = "UZB", "Greenland" = "GRL", "Türkiye" = "TUR",
  "Kosovo" = "KOS", "Indonesia" = "INA", "Malaysia" = "MAS", "Tonga" = "TGA"
)
HOME_NATIONS <- c("ENG", "SCO", "WAL", "NIR")

# Flag (or country code) for the header. Uses birth country, else the first passport.
flag_tag <- function(birth, passport) {
  country <- if (!is.na(birth) && nzchar(birth)) birth else trimws(strsplit(passport %||% "", ",")[[1]][1])
  if (is.null(country) || is.na(country) || !nzchar(country)) return(NULL)
  code <- unname(COUNTRY_CODES[country])
  if (is.na(code)) return(span(class = "flag flag-code", title = country, toupper(substr(country, 1, 3))))
  file <- paste0(code, ".png")
  if (!file.exists(file.path("www", FLAG_DIR, file)) && code %in% HOME_NATIONS) file <- "GBR.png"
  if (!file.exists(file.path("www", FLAG_DIR, file))) return(span(class = "flag flag-code", title = country, code))
  tags$img(class = "flag", src = file.path(FLAG_DIR, file), alt = country, title = country)
}

# ---- Load data once at startup ----------------------------------------------
# If a file isn't where the settings say, look for it in the app folder, the
# folder above, the one above that, and the current working directory
# (including their subfolders), and use the first match.
find_file <- function(path, label) {
  if (file.exists(path)) return(normalizePath(path))
  target <- basename(path)
  parent <- basename(dirname(path))          # e.g. "GAR_output_real" for the GAR files
  roots <- unique(normalizePath(c(".", "..", "../..", getwd()), mustWork = FALSE))
  for (r in roots) {
    hits <- list.files(r, pattern = paste0("^", gsub(".", "\\.", target, fixed = TRUE), "$"),
                       recursive = TRUE, full.names = TRUE)
    if (parent %in% c("GAR_output_real", "par_output_real")) hits <- hits[basename(dirname(hits)) == parent]
    hits <- hits[!grepl("recruiting_app", hits, fixed = TRUE)]
    if (length(hits)) {
      hit <- normalizePath(hits[which.min(nchar(hits))])
      message("Using ", label, ": ", hit)
      return(hit)
    }
  }
  stop("Can't find ", target, " (looked for ", normalizePath(path, mustWork = FALSE),
       " and in the folders around the app). Set GAR_DIR / METRICS_FILE at the top of app.R ",
       "to the full path, e.g. \"C:/Users/you/Documents/project/GAR_output\".", call. = FALSE)
}
gar_file <- find_file(file.path(GAR_DIR, "player_gar.csv"), "GAR ratings")
GAR_DIR  <- dirname(gar_file)
for (f in c("portal_vs_roster.csv", "our_roster.csv"))
  if (!file.exists(file.path(GAR_DIR, f)))
    stop(f, " is missing from ", GAR_DIR, ". Re-run player_gar.R to create it.", call. = FALSE)
METRICS_FILE <- find_file(METRICS_FILE, "player metrics")

gar <- read_csv(file.path(GAR_DIR, "player_gar.csv"), show_col_types = FALSE)
# PAR (points above replacement) from player_war.R with OUTCOME <- "points". Optional:
# without it the app shows GAR only.
par_file <- tryCatch(find_file(file.path(PAR_DIR, "player_par.csv"), "PAR ratings"), error = function(e) NA)
HAS_PAR <- !is.na(par_file)
if (!HAS_PAR) message("No player_par.csv found: run player_war.R with OUTCOME <- \"points\" to add PAR.")
par <- if (HAS_PAR) read_csv(par_file, show_col_types = FALSE) %>%
  select(season_year, player_id, par, par_per_90, par_rank_pos, any_of(c(par_se = "se_per_90"))) else
  tibble(season_year = numeric(), player_id = character(), par = numeric(),
         par_per_90 = numeric(), par_rank_pos = numeric())
if (!"par_se" %in% names(par)) par$par_se <- NA_real_   # older outputs have no PAR ranges
portal <- read_csv(file.path(GAR_DIR, "portal_vs_roster.csv"), show_col_types = FALSE)
roster <- read_csv(file.path(GAR_DIR, "our_roster.csv"), show_col_types = FALSE)

# Stat-by-stat breakdowns for the "How it's calculated" tab (written by player_gar.R and
# player_war.R; older outputs don't have them and the tab says to re-run the scripts)
read_opt <- function(dir, f) if (!is.na(dir) && file.exists(file.path(dir, f))) read_csv(file.path(dir, f), show_col_types = FALSE) else NULL
PAR_OUT  <- if (HAS_PAR) dirname(par_file) else NA
calc <- list(
  gar = list(br = read_opt(GAR_DIR, "gar_breakdown.csv"), repl = read_opt(GAR_DIR, "gar_replacement.csv"),
             set = read_opt(GAR_DIR, "model_settings.csv")),
  par = list(br = read_opt(PAR_OUT, "par_breakdown.csv"), repl = read_opt(PAR_OUT, "par_replacement.csv"),
             set = read_opt(PAR_OUT, "model_settings.csv")))
setting <- function(m, nm, default = NA) {
  s <- calc[[m]]$set
  v <- if (!is.null(s)) s$value[s$setting == nm] else character(0)
  if (length(v)) v[1] else default
}
STAT_LABELS <- c(
  xg = "Non-penalty xG / 90", xa = "xA / 90 (chances set up)",
  def_duel_win_pct = "Defensive duels won %", aerial_win_pct = "Aerial duels won %",
  interceptions = "Interceptions (per 30 min of opp. possession)",
  sliding_tackles = "Sliding tackles (per 30 min of opp. possession)",
  shots_blocked = "Shots blocked / 90", cards = "Cards / 90 (yellow + 2 x red)",
  prevented_goals = "Goals prevented / 90 (xG faced - conceded)", exits = "Exits / 90",
  pass_acc = "Pass accuracy %", passes = "Passes / 90", crosses = "Crosses / 90",
  dribbles_won = "Dribbles completed / 90", duels_won = "Offensive + aerial duels won / 90",
  accelerations = "Accelerations / 90", through_passes = "Through passes / 90", key_passes = "Key passes / 90",
  duel_win_pct = "Duels won %", save_pct = "Save rate %", shots_against = "Shots faced / 90",
  clearances = "Clearances / 90", pressing = "Pressing duels / 90", loose_ball_duels = "Loose-ball duels / 90",
  lost_balls = "Ball losses / 90", def_duels_won = "Defensive duels won / 90", aerial_won = "Aerial duels won / 90",
  long_pass_acc = "Long pass accuracy %", prog_passes = "Progressive passes / 90",
  final_third_passes = "Passes into the final third / 90", prog_runs = "Progressive runs / 90",
  box_passes = "Passes into the box / 90", touches_box = "Touches in the box / 90",
  off_duel_win_pct = "Offensive duels won %")
# GAR's attack rows are the two halves of the chance split, not plain stats
GAR_ATTACK_LABELS <- c(xg = "Shooting: his share of his own non-penalty xG",
                       xa = "Passing: his share of the xG he set up (xA)")
stat_label <- function(s) unname(ifelse(s %in% names(STAT_LABELS), STAT_LABELS[s], s))

missing_cols <- setdiff(SKILLS$col, names(read_csv(METRICS_FILE, n_max = 0, show_col_types = FALSE)))
if (length(missing_cols)) SKILLS <- filter(SKILLS, !col %in% missing_cols)

metrics <- read_csv(METRICS_FILE, show_col_types = FALSE,
                    col_select = any_of(c("season_year", "team", "player_id", "position",
                                          "age", "height_cm", "foot", "portal_status", "portal_initiated",
                                          "market_value", "birth_country", "passport_country", SKILLS$col)))
names(metrics) <- recode(names(metrics), !!!setNames(SKILLS$id, SKILLS$col))
metrics <- distinct(metrics, season_year, player_id, .keep_all = TRUE)

# Roster comparison for ROSTER_SEASON (portal file has no id, so join on name/team/position)
portal_join <- portal %>%
  transmute(player_name, team, position, season_year = ROSTER_SEASON,
            benchmark_player, benchmark_per_90, chance_better,
            season_upgrade, season_upgrade_lo, season_upgrade_hi)

players <- gar %>%
  left_join(metrics %>% select(season_year, player_id, age, height_cm, foot,
                               any_of(c("portal_status", "portal_initiated", "market_value",
                                        "birth_country", "passport_country"))),
            by = c("season_year", "player_id")) %>%
  left_join(portal_join, by = c("player_name", "team", "position", "season_year")) %>%
  left_join(par, by = c("season_year", "player_id")) %>%
  mutate(search = tolower(paste(player_name, team, conference)))
for (nm in c("portal_status", "portal_initiated", "market_value", "birth_country", "passport_country"))
  if (!nm %in% names(players)) players[[nm]] <- NA
HAS_PORTAL <- any(!is.na(players$portal_status))
HAS_CLASS  <- any(!is.na(players$class_year))

# The rating the board leads with: PAR when it's there, otherwise GAR. It orders the
# list and picks our weakest / best regular, and "Chance better" uses it.
LEAD <- if (HAS_PAR) "par" else "gar"

# Our regulars (the players with the most minutes at each position, from player_gar.R),
# then our weakest regular at each position (the benchmark) and our best one
regulars <- roster %>%
  filter(regular) %>%
  select(position, player_name) %>%
  inner_join(players %>% filter(season_year == ROSTER_SEASON, team == FOCUS_TEAM) %>%
               select(player_name, position, player_id, gar_per_90, se_per_90, par_per_90, par_se),
             by = c("player_name", "position"))
lead90 <- paste0(LEAD, "_per_90")
benchmarks    <- regulars %>% group_by(position) %>% slice_min(.data[[lead90]], n = 1, with_ties = FALSE) %>% ungroup()
best_regulars <- regulars %>% group_by(position) %>% slice_max(.data[[lead90]], n = 1, with_ties = FALSE) %>% ungroup()

# Chance a player rates above our weakest regular at his position, from PAR and its
# 80% ranges (GAR's version from portal_vs_roster.csv when there's no PAR)
if (LEAD == "par") {
  players <- players %>%
    left_join(benchmarks %>% select(position, b90 = par_per_90, bse = par_se), by = "position") %>%
    mutate(chance_better = if_else(season_year == ROSTER_SEASON & team != FOCUS_TEAM & minutes_played >= COMPARE_MIN,
                                   pnorm((par_per_90 - b90) / sqrt(par_se^2 + bse^2)), NA_real_)) %>%
    select(-b90, -bse)
}

SEASONS <- sort(unique(players$season_year), decreasing = TRUE)

# Key skills use non-penalty xG when the data has it
if ("npxg_90" %in% SKILLS$id) KEY_SKILLS <- lapply(KEY_SKILLS, function(k) sub("^xg_90$", "npxg_90", k))

# ---- Season stats (the Season stats tab) ------------------------------------------
# One row per stat: its total (tot) and per-90 (p90) columns in the export's Raw sheet.
# kind: count (shown as a total or per 90), pct (a percentage),
# info (shown as is). gk: TRUE = goalkeepers only, FALSE = outfield only, NA = everyone.
STAT_ROWS <- tribble(
  ~label, ~group, ~kind, ~tot, ~p90, ~gk,
  "Matches played", "Playing time", "info", "Matches played", NA, NA,
  "Minutes", "Playing time", "info", "Minutes played", NA, NA,
  "Goals", "Scoring", "count", "Goals", "Goals per 90", FALSE,
  "Non-penalty goals", "Scoring", "count", "Non-penalty goals", "Non-penalty goals per 90", FALSE,
  "Penalties", "Scoring", "pens", "Penalties taken", "Penalty conversion, %", FALSE,
  "xG", "Scoring", "count", "xG", "xG per 90", FALSE,
  "Non-penalty xG", "Scoring", "npxg", NA, NA, FALSE,
  "Shots", "Scoring", "count", "Shots", "Shots per 90", FALSE,
  "Shots on target %", "Scoring", "pct", NA, "Shots on target, %", FALSE,
  "Goal conversion %", "Scoring", "pct", NA, "Goal conversion, %", FALSE,
  "Headed goals", "Scoring", "count", "Head goals", "Head goals per 90", FALSE,
  "Touches in the box", "Scoring", "count", NA, "Touches in box per 90", FALSE,
  "Assists", "Creating", "count", "Assists", "Assists per 90", NA,
  "xA", "Creating", "count", "xA", "xA per 90", NA,
  "Shot assists", "Creating", "count", NA, "Shot assists per 90", FALSE,
  "Key passes", "Creating", "count", NA, "Key passes per 90", FALSE,
  "Second assists", "Creating", "count", NA, "Second assists per 90", FALSE,
  "Through passes", "Creating", "count", NA, "Through passes per 90", FALSE,
  "Passes into the box", "Creating", "count", NA, "Passes to penalty area per 90", FALSE,
  "Crosses", "Creating", "count", NA, "Crosses per 90", FALSE,
  "Cross accuracy %", "Creating", "pct", NA, "Accurate crosses, %", FALSE,
  "Passes", "Passing", "count", NA, "Passes per 90", NA,
  "Pass accuracy %", "Passing", "pct", NA, "Accurate passes, %", NA,
  "Progressive passes", "Passing", "count", NA, "Progressive passes per 90", NA,
  "Passes into the final third", "Passing", "count", NA, "Passes to final third per 90", NA,
  "Long passes", "Passing", "count", NA, "Long passes per 90", NA,
  "Long pass accuracy %", "Passing", "pct", NA, "Accurate long passes, %", NA,
  "Dribbles", "Carrying", "count", NA, "Dribbles per 90", FALSE,
  "Dribble success %", "Carrying", "pct", NA, "Successful dribbles, %", FALSE,
  "Progressive runs", "Carrying", "count", NA, "Progressive runs per 90", FALSE,
  "Defensive duels", "Defending", "count", NA, "Defensive duels per 90", FALSE,
  "Defensive duels won %", "Defending", "pct", NA, "Defensive duels won, %", FALSE,
  "Aerial duels", "Defending", "count", NA, "Aerial duels per 90", FALSE,
  "Aerial duels won %", "Defending", "pct", NA, "Aerial duels won, %", FALSE,
  "Interceptions", "Defending", "count", NA, "Interceptions per 90", FALSE,
  "Sliding tackles", "Defending", "count", NA, "Sliding tackles per 90", FALSE,
  "Shots blocked", "Defending", "count", NA, "Shots blocked per 90", FALSE,
  "Successful defensive actions", "Defending", "count", NA, "Successful defensive actions per 90", FALSE,
  "Fouls", "Discipline", "count", NA, "Fouls per 90", NA,
  "Yellow cards", "Discipline", "count", "Yellow cards", "Yellow cards per 90", NA,
  "Red cards", "Discipline", "count", "Red cards", "Red cards per 90", NA,
  "Goals conceded", "Goalkeeping", "count", "Conceded goals", "Conceded goals per 90", TRUE,
  "Shots faced", "Goalkeeping", "count", "Shots against", "Shots against per 90", TRUE,
  "Save rate %", "Goalkeeping", "pct", NA, "Save rate, %", TRUE,
  "xG faced", "Goalkeeping", "count", "xG against", "xG against per 90", TRUE,
  "Goals prevented (xG faced - conceded)", "Goalkeeping", "count", "Prevented goals", "Prevented goals per 90", TRUE,
  "Clean sheets", "Goalkeeping", "count", "Clean sheets", NA, TRUE,
  "Exits", "Goalkeeping", "count", NA, "Exits per 90", TRUE
)
PEN_XG <- 0.76   # xG of a penalty (same as prepare_real_data.R)
stats_file <- tryCatch(find_file(STATS_FILE, "season stats"), error = function(e) NA)
stat_cols <- unique(na.omit(c(STAT_ROWS$tot, STAT_ROWS$p90, "Penalties taken", "Penalty conversion, %", "xG")))
season_stats <- if (!is.na(stats_file)) {
  read_csv(stats_file, show_col_types = FALSE, guess_max = 10000,
           col_select = any_of(c("season_year", "player_id", "team", "position",
                                 "Minutes played", "minutes_played", stat_cols)))
} else tibble(season_year = numeric(), player_id = character())
if (!"minutes_played" %in% names(season_stats) && "Minutes played" %in% names(season_stats))
  season_stats$minutes_played <- season_stats[["Minutes played"]]

# One player's season stats as a stat x season table
stat_table <- function(pid, mode = "tot", is_gk = FALSE) {
  d <- season_stats %>% filter(player_id == pid) %>% arrange(desc(season_year))
  if (!nrow(d)) return(NULL)
  num <- function(df, col) if (!is.na(col) && col %in% names(df)) suppressWarnings(as.numeric(df[[col]])) else rep(NA_real_, nrow(df))
  rows <- STAT_ROWS %>% filter(is.na(gk) | gk == is_gk)
  out <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]; tc <- r$tot; pc <- r$p90
    mins <- d$minutes_played
    v <- switch(r$kind,
      info = num(d, tc),
      pct  = num(d, pc),
      pens = num(d, tc),   # penalties taken (shown below as scored/taken)
      npxg = {
        xg_t <- coalesce(num(d, "xG"), num(d, "xG per 90") * mins / 90)
        tot <- pmax(xg_t - PEN_XG * coalesce(num(d, "Penalties taken"), 0), 0)
        if (mode == "tot") tot else tot * 90 / mins
      },
      count = {
        tot <- coalesce(num(d, tc), num(d, pc) * mins / 90)
        p90 <- coalesce(num(d, pc), num(d, tc) * 90 / mins)
        if (mode == "tot") tot else p90
      })
    # Penalties: scored out of taken, e.g. 3/4 (scored = taken x conversion %)
    pen_scored <- if (r$kind == "pens") round(num(d, tc) * num(d, pc) / 100) else NULL
    shown <- vapply(seq_along(v), function(j) {
      x <- v[j]
      if (r$kind == "pens") return(if (is.na(x)) "—" else sprintf("%d/%d", as.integer(coalesce(pen_scored[j], 0)), as.integer(x)))
      if (is.na(x)) return("—")
      if (r$kind == "pct") return(sprintf("%.1f%%", x))
      if (r$kind == "info") return(format(round(x), big.mark = ","))
      expected <- grepl("xG|xA|prevented", r$label)   # expected-goal stats keep decimals
      if (mode == "tot" && !expected) return(format(round(x), big.mark = ","))
      sprintf("%.2f", x)
    }, character(1))
    c(Group = r$group, Stat = r$label, setNames(shown, as.character(d$season_year)))
  })
  tab <- as_tibble(do.call(rbind, out))
  tab[apply(tab[, -(1:2), drop = FALSE], 1, function(x) any(x != "—")), ]
}
CONFS   <- sort(unique(players$conference))

# Percentile of x within a reference vector (0-100), flipped when lower is better
pct_in <- function(x, ref, lower) {
  ref <- ref[!is.na(ref)]
  if (is.na(x) || length(ref) < 5) return(NA_real_)
  p <- 100 * (mean(ref < x) + 0.5 * mean(ref == x))
  if (lower) 100 - p else p
}

fmt <- function(x, d = 2, sign = FALSE, pct = FALSE) {
  if (length(x) == 0 || is.na(x)) return("—")
  if (pct) return(paste0(round(100 * x), "%"))
  formatC(x, format = "f", digits = d, flag = if (sign) "+" else "")
}

players <- players %>% mutate(key = paste(season_year, player_id, sep = "|"))
Z80 <- qnorm(0.9)

theme_board <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(text = element_text(colour = "#3A3A3F"),
          axis.text = element_text(colour = "#6B6B70"),
          axis.title = element_text(colour = "#6B6B70", size = base - 1),
          panel.grid.major = element_line(colour = "#EEEAE6"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = base),
          legend.position = "top", legend.justification = "left",
          legend.margin = margin(0, 0, 0, 0), legend.text = element_text(size = base - 1))
}

# Short definitions shown when hovering over a column or tab name
TIPS <- c(
  GAR = "Goals above replacement: goals a player adds over a replacement-level backup at his position",
  `GAR/90` = "GAR per 90 minutes",
  PAR = "Points above replacement: league points added (3 for a win, 1 for a draw)",
  `Chance better` = sprintf("Chance his %s per 90 is above our weakest regular's at his position (%d+ minutes)",
                            toupper(LEAD), COMPARE_MIN),
  `PAR/90` = "PAR per 90 minutes",
  Portal = "Transfer portal status",
  Min = "Minutes played",
  Pos = "Position", Class = "Class year"
)

# ---- UI ----------------------------------------------------------------------
theme <- bs_theme(version = 5, primary = MAROON, secondary = ORANGE,
                  bg = "#FFFFFF", fg = "#1F1F23",
                  base_font = font_google("Inter", local = FALSE),
                  heading_font = font_google("Barlow Semi Condensed", wght = c(500, 600, 700), local = FALSE),
                  "border-radius" = "8px", "font-size-base" = "0.92rem")

CSS <- "
  :root { --vt-maroon: #861F41; --vt-maroon-dark: #6A1833; --vt-orange: #E5751F;
          --ink: #1F1F23; --muted: #6B6B70; --line: #E6E2DE; --soft: #F5F3F0; }
  body { background: var(--soft); }

  /* Header bar */
  .bslib-page-title, .navbar { background: var(--vt-maroon) !important; color: #fff !important;
    border-bottom: 3px solid var(--vt-orange) !important; padding: 10px 20px !important; }
  .brand { display: flex; align-items: center; justify-content: space-between; width: 100%;
    background: var(--vt-maroon); color: #fff; padding: 10px 22px; border-bottom: 3px solid var(--vt-orange); }
  .bslib-page-title:has(.brand) { padding: 0 !important; background: var(--vt-maroon) !important; border: 0 !important; }
  .brand-left { display: flex; align-items: center; gap: 12px; }
  /* www/vt_logo.png on a small white badge so its maroon fill stands out from the header */
  .brand-logo { height: 34px; width: auto; display: block; background: #fff; padding: 4px 8px; border-radius: 6px; }
  .brand-name { font-family: 'Barlow Semi Condensed', sans-serif; font-weight: 600; font-size: 1.35rem;
    letter-spacing: .02em; color: #fff; }
  .brand-team { font-size: .8rem; color: rgba(255,255,255,.75); letter-spacing: .04em; text-transform: uppercase; }

  /* Sidebar */
  .bslib-sidebar-layout > .sidebar { background: #fff; border-right: 1px solid var(--line); }
  .sidebar .control-label, .sidebar .form-label, .sec-label {
    font-size: .7rem; font-weight: 600; text-transform: uppercase; letter-spacing: .07em; color: var(--muted); }
  .form-control, .selectize-input { border-color: var(--line); border-radius: 6px; box-shadow: none; }
  .form-control:focus, .selectize-input.focus { border-color: var(--vt-maroon);
    box-shadow: 0 0 0 3px rgba(134,31,65,.12); }
  .form-check-input:checked { background-color: var(--vt-maroon); border-color: var(--vt-maroon); }
  .form-check-input:focus { box-shadow: 0 0 0 3px rgba(134,31,65,.15); border-color: var(--vt-maroon); }
  .pos-box .shiny-options-group { display: grid; grid-template-columns: repeat(4, 1fr); column-gap: 4px; }
  .pos-box .checkbox, .pos-box .form-check { margin: 0 0 2px; font-size: .86rem; }
  .pos-box { margin-bottom: 14px; }
  .pos-box .form-group, .pos-box .shiny-input-container { margin-bottom: 0; }
  .pos-links a { color: var(--vt-maroon); text-decoration: none; font-size: .75rem; font-weight: 600; }
  .irs--shiny .irs-bar { background: var(--vt-maroon); border-color: var(--vt-maroon); }
  .irs--shiny .irs-single { background: var(--vt-maroon); }
  .irs--shiny .irs-handle { border-color: var(--vt-maroon); }
  .irs--shiny .irs-min, .irs--shiny .irs-max { background: var(--soft); color: var(--muted); }
  .btn-vt { background: var(--vt-maroon); border-color: var(--vt-maroon); color: #fff; }
  .btn-vt:hover { background: var(--vt-maroon-dark); border-color: var(--vt-maroon-dark); color: #fff; }
  .btn-quiet { background: #fff; border: 1px solid var(--line); color: var(--ink); }
  .btn-quiet:hover { border-color: var(--vt-maroon); color: var(--vt-maroon); background: #fff; }

  /* Cards */
  .card { border: 1px solid var(--line); border-radius: 10px; box-shadow: none; }
  .card-header { background: #fff; border-bottom: 1px solid var(--line); }
  .list-head { font-size: .7rem; font-weight: 600; text-transform: uppercase; letter-spacing: .07em; color: var(--muted); }

  /* Player list */
  table.dataTable { font-size: .83rem; }
  table.dataTable thead th { font-size: .68rem; font-weight: 600; text-transform: uppercase; letter-spacing: .05em;
    color: var(--muted); border-bottom: 1px solid var(--line) !important; cursor: help; }
  table.dataTable th, table.dataTable td { white-space: nowrap; }
  table.dataTable tbody tr { cursor: pointer; }
  table.dataTable tbody tr:hover > * { background: #FBF4F6 !important; }
  table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px var(--vt-maroon) !important; color: #fff !important; }
  table.dataTable.no-footer { border-bottom: 1px solid var(--line); }

  /* Player header */
  .who { width: 100%; }
  .who-name { display: flex; align-items: center; flex-wrap: wrap; gap: 10px;
    font-family: 'Barlow Semi Condensed', sans-serif; font-size: 1.75rem; font-weight: 700;
    line-height: 1.1; margin: 0; color: var(--ink); }
  .flag { height: 20px; width: auto; border-radius: 3px; flex: none;
    box-shadow: 0 0 0 1px rgba(0,0,0,.12); }
  .flag-code { display: inline-flex; align-items: center; height: 20px; padding: 0 6px; background: var(--soft);
    color: var(--muted); font-family: Inter, sans-serif; font-size: .66rem; font-weight: 600; letter-spacing: .05em; }
  .who-sub { color: var(--muted); font-size: .9rem; font-weight: 400; margin-top: 4px; }
  .who-seasons { color: var(--muted); font-size: .82rem; font-weight: 400; margin-top: 2px; }
  .who-seasons .season-on { color: var(--ink); font-weight: 600; margin-left: 4px; }
  .who-seasons .season-off { color: var(--vt-maroon); text-decoration: none; margin-left: 4px; }
  .who-seasons .season-off:hover { text-decoration: underline; }
  .chip { display: inline-block; font-family: Inter, sans-serif; font-size: .68rem; font-weight: 600;
    padding: 2px 9px; border-radius: 999px; letter-spacing: .02em; }
  .chip-ours { background: var(--vt-maroon); color: #fff; }
  .kpis { display: flex; flex-wrap: wrap; gap: 0; margin: 12px 0 2px; }
  .kpis > div { padding: 0 22px; border-left: 1px solid var(--line); }
  .kpis > div:first-child { padding-left: 0; border-left: 0; }
  .kpi-v { font-family: 'Barlow Semi Condensed', sans-serif; font-size: 1.7rem; font-weight: 600; line-height: 1.05;
    font-variant-numeric: tabular-nums; color: var(--ink); }
  .kpi-l { font-size: .68rem; text-transform: uppercase; letter-spacing: .06em; color: var(--muted); margin-top: 2px; }

  /* Comparison controls and tabs */
  .cmp-row { display: flex; flex-wrap: wrap; gap: 18px; align-items: end; margin-bottom: 6px; }
  .cmp-row .form-group { margin-bottom: 0; }
  .cmp-row .control-label { font-size: .7rem; font-weight: 600; text-transform: uppercase; letter-spacing: .07em; color: var(--muted); }
  .nav-underline { border-bottom: 1px solid var(--line); gap: 22px; margin: 6px 0 12px; }
  .nav-underline .nav-link { color: var(--muted); font-weight: 600; font-size: .86rem; padding: 8px 0; }
  .nav-underline .nav-link.active, .nav-underline .nav-link:hover { color: var(--vt-maroon); border-bottom-color: var(--vt-maroon); }
  .sec-title { font-family: 'Barlow Semi Condensed', sans-serif; font-size: 1.05rem; font-weight: 600;
    color: var(--ink); margin: 16px 0 2px; }

  /* Head-to-head numbers */
  .verdict { display: grid; gap: 10px; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); margin: 4px 0 8px; }
  .vd { background: var(--soft); border-radius: 8px; padding: 10px 14px; border-top: 3px solid var(--vt-maroon); }
  .vd-v { font-family: 'Barlow Semi Condensed', sans-serif; font-size: 1.45rem; font-weight: 600; font-variant-numeric: tabular-nums; }
  .vd-l { font-size: .74rem; color: var(--muted); }
  .note { font-size: .78rem; color: var(--muted); }

  /* How it's calculated */
  .eq { display: flex; flex-wrap: wrap; align-items: stretch; gap: 6px; margin: 4px 0 10px; }
  .eq-term { background: var(--soft); border-radius: 8px; padding: 8px 12px; min-width: 92px; }
  .eq-term .v { font-family: 'Barlow Semi Condensed', sans-serif; font-size: 1.3rem; font-weight: 600;
    font-variant-numeric: tabular-nums; }
  .eq-term .l { font-size: .72rem; color: var(--muted); }
  .eq-term.total { background: #fff; border: 1px solid var(--line); }
  .eq-op { align-self: center; color: var(--muted); font-size: 1.1rem; padding: 0 2px; }
  .steps { padding-left: 1.2rem; font-size: .86rem; line-height: 1.5; }
  .steps li { margin-bottom: 8px; }
  .steps b { color: var(--ink); }

  /* Player info */
  .facts { display: grid; grid-template-columns: repeat(auto-fit, minmax(130px, 1fr)); gap: 12px 18px; font-size: .88rem; }
  .facts b { display: block; font-weight: 600; color: var(--ink); }
  .facts span { color: var(--muted); font-size: .68rem; text-transform: uppercase; letter-spacing: .06em; }
"

ui <- page_sidebar(
  window_title = "VT Portal Recruiting Board",
  title = div(class = "brand",
              div(class = "brand-left", tags$img(src = "vt_logo.png", class = "brand-logo", alt = "Virginia Tech"),
                  span(class = "brand-name", "Portal Recruiting Board")),
              span(class = "brand-team", "Virginia Tech Men's Soccer")),
  theme = theme,
  fillable = FALSE,
  tags$head(tags$style(HTML(CSS))),
  sidebar = sidebar(
    width = 270,
    textInput("q", "Search", placeholder = "Name, team or conference"),
    selectInput("season", "Season", choices = SEASONS, selected = ROSTER_SEASON),
    div(class = "pos-box",
        div(class = "d-flex justify-content-between align-items-baseline",
            tags$label(class = "control-label", "Positions"),
            div(class = "pos-links", actionLink("pos_all", "All"), " · ", actionLink("pos_none", "None"))),
        checkboxGroupInput("pos", NULL, choices = names(POS_LABELS), selected = names(POS_LABELS))),
    selectInput("conf", "Conference", choices = c("All conferences" = "all", CONFS)),
    if (HAS_CLASS) selectInput("cls", "Class", choices = c("All classes" = "all", setNames(names(CLASS_LABELS), CLASS_LABELS))),
    if (HAS_PORTAL) selectInput("portal", "Transfer portal",
                                choices = c("Anyone" = "all", sort(unique(na.omit(players$portal_status))))),
    sliderInput("minmin", "Minimum minutes", min = 0, max = 2500, value = 300, step = 90),
    checkboxInput("upg", "Likely upgrades only"),
    div(class = "d-flex gap-2 mt-1",
        actionButton("reset", "Reset", class = "btn-quiet btn-sm"),
        downloadButton("dl", "Export list", class = "btn-vt btn-sm"))
  ),

  layout_columns(
    col_widths = breakpoints(sm = 12, xl = c(5, 7)),
    card(
      full_screen = TRUE,
      card_header(div(class = "list-head", textOutput("n_match", inline = TRUE))),
      DTOutput("tbl")
    ),
    card(
      full_screen = TRUE,
      card_header(uiOutput("who")),
      card_body(
      div(class = "cmp-row",
          selectizeInput("cmp", "Compare with", choices = NULL, width = "340px"),
          radioButtons("scale", NULL, inline = TRUE,
                       choices = c("Per 90" = "p90", "Season total" = "tot"))),
      navset_underline(
      id = "tab",
      nav_panel("Head to head",
        uiOutput("verdict"),
        plotOutput("gar_cmp", height = "170px"),
        if (HAS_PAR) plotOutput("par_cmp", height = "130px"),
        div(class = "sec-title", title = "Percentile among players at his position this season", "Key skills"),
        plotOutput("radar", height = "380px")
      ),
      nav_panel(span("Skills", title = sprintf(
          "Percentile among same-season players at his position with %d+ minutes. Higher is better (flipped where fewer is better). Hover a dot for the per-90 value.", PCT_MIN)),
        value = "Skills",
        div(class = "cmp-row",
            radioButtons("skmode", NULL, inline = TRUE,
                         choices = c("Key skills" = "key", "Biggest gaps" = "gaps", "All skills" = "all"))),
        plotOutput("dumbbell", height = "auto", hover = hoverOpts("db_hover", delay = 80)),
        uiOutput("db_tip")
      ),
      nav_panel(span("Season stats", title =
          "Actual Wyscout numbers (not conference-adjusted). Totals without a Wyscout total are per 90 x minutes / 90."),
        value = "Season stats",
        div(class = "cmp-row",
            radioButtons("st_mode", NULL, inline = TRUE, choices = c("Season totals" = "tot", "Per 90" = "p90"))),
        DTOutput("stats_tbl")
      ),
      nav_panel(span("How it's calculated", title = "Step by step: how this player's GAR or PAR is built from his stats"),
        value = "calc",
        if (HAS_PAR) div(class = "cmp-row",
            radioButtons("calc_model", NULL, inline = TRUE,
                         choices = c("GAR (goals)" = "gar", "PAR (league points)" = "par"))),
        uiOutput("calc_sum"),
        plotOutput("calc_plot", height = "auto"),
        div(class = "sec-title", "Stat by stat"),
        DTOutput("calc_tbl"),
        div(class = "sec-title", "Step by step"),
        uiOutput("calc_steps")
      ),
      nav_panel("History & info",
        div(class = "sec-title", style = "margin-top: 4px", "GAR per 90 by season"),
        plotOutput("hist", height = "240px"),
        div(class = "sec-title", "Season by season"),
        DTOutput("career_tbl"),
        div(class = "sec-title", "Player info"),
        uiOutput("facts")
      )
    )))
  )
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateTextInput(session, "q", value = "")
    updateSelectInput(session, "season", selected = ROSTER_SEASON)
    for (id in c("conf", "cls", "portal")) updateSelectInput(session, id, selected = "all")
    updateCheckboxGroupInput(session, "pos", selected = names(POS_LABELS))
    updateSliderInput(session, "minmin", value = 300)
    updateCheckboxInput(session, "upg", value = FALSE)
  })

  q_slow <- debounce(reactive(input$q), 300)
  observeEvent(input$pos_all,  updateCheckboxGroupInput(session, "pos", selected = names(POS_LABELS)))
  observeEvent(input$pos_none, updateCheckboxGroupInput(session, "pos", selected = character(0)))
  # A dropdown filters only when something other than its "All ..." option is picked.
  # ("All" options use the value "all", not "": the dropdown hides an option whose value
  # is empty once something else is picked, so you could never get back to it.)
  picked <- function(v) !is.null(v) && length(v) == 1 && nzchar(v) && v != "all"

  filtered <- reactive({
    d <- players %>% filter(season_year == as.numeric(input$season), minutes_played >= input$minmin)
    q <- trimws(tolower(q_slow() %||% ""))
    if (nzchar(q))                    d <- filter(d, grepl(q, search, fixed = TRUE))
    d <- filter(d, position %in% (input$pos %||% character(0)))   # only the checked positions
    if (picked(input$conf))   d <- filter(d, conference == input$conf)
    if (picked(input$cls))    d <- filter(d, class_year == input$cls)
    if (picked(input$portal)) d <- filter(d, portal_status == input$portal)
    if (isTRUE(input$upg))            d <- filter(d, !is.na(chance_better), chance_better >= UPGRADE_MIN)
    arrange(d, desc(.data[[LEAD]]))
  })

  output$n_match <- renderText({
    sprintf("%s players  |  %s season", format(nrow(filtered()), big.mark = ","), input$season)
  })

  output$tbl <- renderDT({
    d <- filtered() %>%
      transmute(Player = player_name, Team = team, Pos = position,
                Class = class_year, Portal = portal_status,
                Min = minutes_played, PAR = par, `PAR/90` = par_per_90, GAR = gar,
                `Chance better` = chance_better)
    if (!HAS_CLASS)  d$Class  <- NULL
    if (!HAS_PORTAL) d$Portal <- NULL
    if (!HAS_PAR)    { d$PAR <- NULL; d$`PAR/90` <- NULL; d$`GAR/90` <- filtered()$gar_per_90 }
    lead_col <- toupper(LEAD)
    # column names with a short definition on hover
    head_tips <- htmltools::withTags(table(class = "display", thead(tr(lapply(names(d), function(n)
      th(n, title = if (n %in% names(TIPS)) TIPS[[n]] else NULL))))))
    datatable(
      d, rownames = FALSE, container = head_tips, selection = list(mode = "single", selected = 1),
      class = "compact hover nowrap",
      options = list(dom = "t", paging = FALSE, deferRender = TRUE, scrollY = "640px", scrollCollapse = TRUE,
                     scrollX = TRUE, order = list(list(which(names(d) == lead_col) - 1, "desc")),
                     columnDefs = list(list(className = "dt-left", targets = 0:(which(names(d) == "Min") - 2))))
    ) %>%
      formatRound("GAR", 2) %>%
      formatRound(intersect(c("GAR/90", "PAR/90"), names(d)), 3) %>%
      formatRound(intersect("PAR", names(d)), 2) %>%
      formatRound("Min", 0) %>%
      formatPercentage("Chance better", 0) %>%
      formatStyle(lead_col, background = styleColorBar(range(c(0, d[[lead_col]]), na.rm = TRUE), "#EAD3DA"),
                  backgroundSize = "95% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # The selected player: the clicked row, or the top row of the list
  picked_row <- reactive({
    d <- filtered()
    req(nrow(d) > 0)
    i <- input$tbl_rows_selected
    if (length(i) == 0 || i > nrow(d)) i <- (input$tbl_rows_all %||% 1)[1]
    if (is.na(i) || i > nrow(d)) i <- 1
    d[i, ]
  })
  # Season shown in the profile: the list's season, or another season of the same player
  # (the season links in the header). Picking a new player goes back to the list's season.
  prof_season <- reactiveVal(NULL)
  observeEvent(picked_row()$key, prof_season(NULL))
  observeEvent(input$prof_season, prof_season(as.numeric(input$prof_season)))
  sel <- reactive({
    p <- picked_row(); y <- prof_season()
    if (is.null(y) || y == p$season_year) return(p)
    r <- players %>% filter(player_id == p$player_id, season_year == y)
    if (nrow(r)) r[1, ] else p
  })

  # ---- Comparison player ------------------------------------------------------
  observeEvent(input$season, {
    season_players <- players %>%
      filter(season_year == as.numeric(input$season) | (team == FOCUS_TEAM & season_year == ROSTER_SEASON)) %>%
      arrange(desc(team == FOCUS_TEAM), desc(.data[[LEAD]]))
    ch <- c("Our weakest regular at his position" = "bench",
            "Our best regular at his position" = "best",
            setNames(season_players$key,
                     sprintf("%s — %s, %s (%s)", season_players$player_name, season_players$team,
                             season_players$position, season_players$season_year)))
    keep <- isolate(input$cmp)
    updateSelectizeInput(session, "cmp", choices = ch, server = TRUE,
                         selected = if (!is.null(keep) && keep %in% ch) keep else "bench",
                         options = list(placeholder = "Type a name"))
  })

  comp <- reactive({
    s <- sel()
    v <- input$cmp %||% "bench"
    id <- switch(v,
      bench = benchmarks$player_id[benchmarks$position == s$position][1],
      best  = best_regulars$player_id[best_regulars$position == s$position][1],
      NA)
    row <- if (v %in% c("bench", "best")) players %>% filter(season_year == ROSTER_SEASON, player_id == id)
           else players %>% filter(key == v)
    if (!nrow(row) || row$key[1] == s$key) return(NULL)
    row[1, ] %>% mutate(role = switch(v, bench = sprintf("our weakest regular at %s", s$position),
                                         best = sprintf("our best regular at %s", s$position),
                                         sprintf("%s, %s", team, season_year)))
  })

  comp_msg <- reactive({
    s <- sel(); v <- input$cmp %||% "bench"
    if (!is.null(comp())) return(NULL)
    if (v %in% c("bench", "best") && s$team == FOCUS_TEAM)
      sprintf("He is %s. Pick another player to compare with.", if (v == "bench") "our weakest regular here" else "our best regular here")
    else if (v %in% c("bench", "best")) sprintf("We have no regular at %s to compare with.", s$position)
    else "Pick a different player to compare with."
  })

  # Skill percentiles for any player, against the SELECTED player's position and season
  ref_set <- reactive({
    s <- sel()
    metrics %>%
      semi_join(gar %>% filter(minutes_played >= PCT_MIN), by = c("season_year", "player_id")) %>%
      filter(season_year == s$season_year, position == s$position)
  })
  skill_vals <- function(row, ref, groups) {
    me <- metrics %>% filter(season_year == row$season_year, player_id == row$player_id)
    SKILLS %>% filter(group %in% groups) %>% rowwise() %>%
      mutate(value = if (nrow(me)) me[[id]][1] else NA_real_,
             pct = pct_in(value, ref[[id]], lower)) %>% ungroup()
  }
  skills_both <- reactive({
    s <- sel(); c <- comp(); ref <- ref_set()
    groups <- if (s$position == "GK") GK_GROUPS else setdiff(GROUP_ORDER, "Goalkeeping")
    a <- skill_vals(s, ref, groups) %>% mutate(who = "a")
    b <- if (!is.null(c)) skill_vals(c, ref, groups) %>% mutate(who = "b") else NULL
    bind_rows(a, b) %>% filter(!is.na(pct))
  })

  # Colors for the selected player (a) and the comparison player (b)
  pcols <- reactive({
    s <- sel(); c <- comp()
    a_vt <- s$team == FOCUS_TEAM
    b_vt <- !is.null(c) && c$team == FOCUS_TEAM
    a <- if (a_vt) VT_ORANGE else BLUE
    b <- if (b_vt) VT_ORANGE else BLUE
    if (!is.null(c) && a_vt == b_vt) b <- if (b_vt) VT_ORANGE2 else BLUE2
    c(a = a, b = b)
  })

  # ---- Header -------------------------------------------------------------------
  output$who <- renderUI({
    s <- sel()
    kpi <- function(v, l) div(div(class = "kpi-v", v), div(class = "kpi-l", l))
    career <- players %>% filter(player_id == s$player_id) %>% arrange(season_year)
    prev <- career %>% filter(season_year < s$season_year) %>% slice_tail(n = 1)
    moved <- nrow(prev) && prev$team != s$team
    season_link <- function(y) {
      if (y == s$season_year) return(span(class = "season-on", y))
      tags$a(href = "#", class = "season-off", y,
             onclick = sprintf("Shiny.setInputValue('prof_season', %d, {priority: 'event'}); return false;", y))
    }
    div(class = "who",
      div(class = "who-name",
          flag_tag(s$birth_country, s$passport_country),
          span(s$player_name),
          if (s$team == FOCUS_TEAM) span(class = "chip chip-ours", "Our player")),
      div(class = "who-sub",
          span(title = POS_LABELS[[s$position]], s$position), " · ", s$team,
          if (moved) sprintf(" · from %s (%d)", prev$team, prev$season_year)),
      if (nrow(career) > 1)
        div(class = "who-seasons", "Season ",
            lapply(career$season_year, function(y) list(season_link(y), " "))),
      div(class = "kpis mt-2",
          if (HAS_PAR) kpi(fmt(s$par), "PAR (points)"),
          kpi(fmt(s$gar), "GAR (goals)"),
          kpi(paste0("#", if (HAS_PAR) s$par_rank_pos else s$gar_rank_pos),
              sprintf("of %s %ss by %s", sum(players$season_year == s$season_year & players$position == s$position),
                      s$position, toupper(LEAD))),
          kpi(format(s$minutes_played, big.mark = ","), "minutes"))
    )
  })

  # ---- Head to head -------------------------------------------------------------
  output$verdict <- renderUI({
    s <- sel(); c <- comp()
    if (is.null(c)) return(p(class = "note mt-2", comp_msg()))
    diff <- s$gar_per_90 - c$gar_per_90
    se <- sqrt(s$se_per_90^2 + c$se_per_90^2)
    chance <- pnorm(diff / se)
    goals <- diff * PROJ_MINUTES / 90
    half <- Z80 * se * PROJ_MINUTES / 90
    vd <- function(v, l, col = "inherit") div(class = "vd", div(class = "vd-v", style = sprintf("color:%s", col), v), div(class = "vd-l", l))
    chance_col <- function(p) if (is.na(p)) "inherit" else if (p >= 0.7) "#1E7B3A" else if (p <= 0.3) "#B3261E" else "inherit"
    # the same chance from PAR, with its own uncertainty ranges
    chance_par <- if (HAS_PAR && !anyNA(c(s$par_per_90, c$par_per_90, s$par_se, c$par_se)))
      pnorm((s$par_per_90 - c$par_per_90) / sqrt(s$par_se^2 + c$par_se^2)) else NA
    div(class = "verdict",
        vd(fmt(chance, pct = TRUE), sprintf("chance he's better than %s (GAR)", c$player_name), chance_col(chance)),
        if (!is.na(chance_par))
          vd(fmt(chance_par, pct = TRUE), sprintf("chance he's better than %s (PAR)", c$player_name), chance_col(chance_par)),
        vd(fmt(goals, 1, TRUE), sprintf("Goals Above Replacement over a %s-min season",
                                         format(PROJ_MINUTES, big.mark = ","))),
        if (HAS_PAR && !is.na(s$par_per_90) && !is.na(c$par_per_90))
          vd(fmt((s$par_per_90 - c$par_per_90) * PROJ_MINUTES / 90, 1, TRUE),
             sprintf("Points Above Replacement over a %s-min season", format(PROJ_MINUTES, big.mark = ","))))
  })

  output$par_cmp <- renderPlot({
    s <- sel(); c <- comp()
    p90 <- !identical(input$scale, "tot")
    d <- bind_rows(s, c) %>%
      transmute(name = factor(player_name, levels = rev(unique(player_name))),
                who = c("a", "b")[seq_len(n())],
                v = if (p90) par_per_90 else par)
    validate(need(any(!is.na(d$v)), "No PAR for this player."))
    ggplot(d, aes(v, name, fill = who)) +
      geom_col(width = 0.55) +
      geom_vline(xintercept = 0, colour = "grey45") +
      geom_text(aes(label = fmt_vec(v, if (p90) 3 else 2), hjust = ifelse(v >= 0, -0.2, 1.2)), size = 3.6) +
      scale_fill_manual(values = pcols(), guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0.08, 0.15))) +
      labs(x = if (p90) "PAR per 90: league points above replacement (3 for a win, 1 for a draw)"
               else "PAR this season: league points above replacement (3 for a win, 1 for a draw)",
           y = NULL) +
      theme_board() +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12, face = "bold"))
  })

  output$gar_cmp <- renderPlot({
    s <- sel(); c <- comp()
    rows <- bind_rows(s, c)
    p90 <- !identical(input$scale, "tot")
    d <- rows %>%
      transmute(name = factor(player_name, levels = rev(unique(player_name))),
                Attack = if (p90) off_per_90 else off_gaa,
                Defense = if (p90) def_per_90 else def_gaa,
                Replacement = if (p90) repl_value * 90 / minutes_played else repl_value,
                total = if (p90) gar_per_90 else gar,
                lo = if (p90) gar_per_90_lo else gar_lo,
                hi = if (p90) gar_per_90_hi else gar_hi)
    long <- d %>% select(name, Attack, Defense, Replacement) %>%
      pivot_longer(-name, names_to = "part", values_to = "v") %>%
      mutate(part = factor(part, levels = c("Replacement", "Defense", "Attack")))
    ggplot(long, aes(y = name)) +
      geom_col(aes(x = v, fill = part), width = 0.55) +
      geom_vline(xintercept = 0, colour = "grey45") +
      geom_errorbarh(data = d, aes(xmin = lo, xmax = hi), height = 0.22, linewidth = 0.6) +
      geom_point(data = d, aes(x = total), shape = 21, size = 3.2, fill = "white", stroke = 1) +
      geom_text(data = d, aes(x = hi, label = fmt_vec(total, if (p90) 3 else 2)), hjust = -0.25, size = 3.6) +
      scale_fill_manual(values = c(Attack = MAROON, Defense = DEFENSE_COL, Replacement = GREY),
                        breaks = c("Attack", "Defense", "Replacement"),
                        labels = c("Attack", "Defense", "Average over replacement")) +
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) +
      labs(x = if (p90) "GAR per 90 (dot = total, line = 80% range)" else "GAR this season (dot = total, line = 80% range)",
           y = NULL, fill = NULL) +
      theme_board() +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 12, face = "bold"))
  })

  output$radar <- renderPlot({
    s <- sel(); c <- comp()
    keys <- intersect(KEY_SKILLS[[s$position]], SKILLS$id)
    d <- skills_both() %>% filter(id %in% keys) %>% mutate(id = factor(id, levels = keys)) %>% arrange(id)
    keys <- intersect(keys, unique(as.character(d$id[d$who == "a"])))
    validate(need(length(keys) >= 3, "Not enough skill data for a radar."))
    n <- length(keys)
    ang <- setNames(pi / 2 - 2 * pi * (seq_len(n) - 1) / n, keys)
    d <- d %>% filter(id %in% keys) %>%
      mutate(a = ang[as.character(id)], x = pct * cos(a), y = pct * sin(a))
    rings <- expand.grid(r = c(25, 50, 75, 100), t = seq(0, 2 * pi, length.out = 120)) %>%
      mutate(x = r * cos(t), y = r * sin(t))
    spokes <- tibble(id = keys, a = ang, x = 100 * cos(a), y = 100 * sin(a),
                     lx = 122 * cos(a), ly = 122 * sin(a),
                     lab = SKILLS$label[match(keys, SKILLS$id)] %>% sub(" per 90", "/90", .) %>%
                       sub(" \\(fewer better\\)", "", .) %>% sub("Interceptions per 30 opp. poss. min", "Interceptions (poss. adj.)", .))
    poly <- d %>% group_by(who) %>% arrange(id, .by_group = TRUE)
    ggplot() +
      geom_path(data = rings, aes(x, y, group = r), colour = "#E6E1DC") +
      geom_segment(data = spokes, aes(x = 0, y = 0, xend = x, yend = y), colour = "#E6E1DC") +
      annotate("text", x = 2, y = c(25, 50, 75, 100) + 4, label = c(25, 50, 75, 100), size = 2.8, colour = "grey55", hjust = 0) +
      geom_polygon(data = poly, aes(x, y, group = who, fill = who, colour = who), alpha = 0.18, linewidth = 1) +
      geom_point(data = poly, aes(x, y, colour = who), size = 2) +
      geom_text(data = spokes, aes(lx, ly, label = lab), size = 3.3, lineheight = 0.9,
                hjust = ifelse(abs(cos(spokes$a)) < 0.2, 0.5, ifelse(cos(spokes$a) > 0, 0, 1))) +
      scale_fill_manual(values = pcols(), guide = "none") +
      scale_colour_manual(values = pcols(), guide = "none") +
      coord_equal(xlim = c(-175, 175), ylim = c(-125, 125), clip = "off") +
      theme_void()
  })

  # ---- Skills tab ----------------------------------------------------------------
  db_data <- reactive({
    s <- sel(); c <- comp()
    d <- skills_both()
    mode <- input$skmode %||% "key"
    wide <- d %>% select(id, label, group, who, pct, value) %>%
      pivot_wider(names_from = who, values_from = c(pct, value))
    if (!"pct_b" %in% names(wide)) { wide$pct_b <- NA_real_; wide$value_b <- NA_real_ }
    wide <- wide %>% filter(!is.na(pct_a))
    if (mode == "key") {
      keys <- intersect(KEY_SKILLS[[s$position]], wide$id)
      wide <- wide %>% filter(id %in% keys) %>% mutate(group = "Key skills")
    } else if (mode == "gaps") {
      wide <- wide %>% mutate(gap = abs(pct_a - coalesce(pct_b, 50))) %>%
        slice_max(gap, n = 12, with_ties = FALSE) %>% arrange(desc(pct_a - coalesce(pct_b, 50))) %>%
        mutate(group = "Biggest gaps")
    }
    wide %>% mutate(group = factor(group, levels = unique(c("Key skills", "Biggest gaps", GROUP_ORDER))),
                    y = factor(paste(group, label, sep = "::"), levels = rev(unique(paste(group, label, sep = "::")))))
  })

  output$dumbbell <- renderPlot({
    d <- db_data()
    validate(need(nrow(d) > 0, "No skill data for this player."))
    ggplot(d, aes(y = y)) +
      geom_vline(xintercept = 50, colour = "#E6E1DC") +
      geom_segment(aes(x = pct_b, xend = pct_a, yend = y), colour = "#CFC8C1", linewidth = 2.2, na.rm = TRUE,
                   lineend = "round") +
      geom_point(aes(x = pct_b), colour = pcols()[["b"]], size = 3.4, na.rm = TRUE) +
      geom_point(aes(x = pct_a), colour = pcols()[["a"]], size = 3.6) +
      facet_grid(group ~ ., scales = "free_y", space = "free_y") +
      scale_y_discrete(labels = function(x) sub("^.*::", "", x)) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = expansion(add = 2)) +
      labs(x = "Percentile at his position", y = NULL) +
      theme_board() +
      theme(panel.grid.major.y = element_blank(),
            strip.text.y = if (identical(input$skmode, "all")) element_text(angle = 0, face = "bold", hjust = 0) else element_blank(),
            axis.text.y = element_text(size = 11), panel.spacing.y = unit(8, "pt"))
  }, height = function() 70 + 26 * nrow(db_data()) + 10 * length(unique(db_data()$group)))

  output$db_tip <- renderUI({
    h <- input$db_hover
    if (is.null(h)) return(p(class = "note", " "))
    d <- db_data()
    yl <- levels(d$y)[round(h$y)]
    r <- d %>% filter(as.character(y) == yl)
    if (!nrow(r)) return(p(class = "note", " "))
    f <- function(v, id) if (is.na(v)) "—" else if (grepl("_pct$", id)) sprintf("%.1f%%", v) else sprintf("%.2f", v)
    s <- sel(); c <- comp()
    p(class = "note", tags$b(r$label), ": ",
      sprintf("%s %s (%s pct)", s$player_name, f(r$value_a, r$id), round(r$pct_a)),
      if (!is.null(c)) sprintf("\u00b7 %s %s (%s pct)", c$player_name, f(r$value_b, r$id), round(r$pct_b)))
  })

  # ---- How it's calculated -------------------------------------------------------
  # The selected player's rating taken apart: every stat's credit vs an average player
  # at his position, plus the gap between average and replacement level. The parts add
  # up to his rating per 90; times minutes / 90 gives the season total.
  calc_model <- reactive(if (HAS_PAR) input$calc_model %||% "gar" else "gar")
  calc_data <- reactive({
    s <- sel(); m <- calc_model(); cm <- calc[[m]]
    validate(need(!is.null(cm$br) && !is.null(cm$repl),
                  sprintf("Re-run %s to add the breakdown.", if (m == "gar") "player_gar.R" else "player_war.R")))
    rp <- cm$repl %>% filter(season_year == s$season_year, position == s$position)
    total90 <- if (m == "gar") s$gar_per_90 else s$par_per_90
    validate(need(nrow(rp) == 1 && !is.na(total90), "No breakdown for this player."))
    rows <- cm$br %>% filter(season_year == s$season_year, player_id == s$player_id)
    rows <- if (m == "gar") {
      mutate(rows, label = if_else(part == "attack", unname(GAR_ATTACK_LABELS[stat]), stat_label(stat))) %>%
        arrange(factor(part, levels = c("attack", "defense")), desc(abs(per_90)))
    } else {
      # PAR: stats grouped into categories; categories in order of their weight
      if (!"category" %in% names(rows)) rows$category <- rows$stat
      mutate(rows, part = category, label = stat_label(stat)) %>%
        group_by(part) %>% mutate(cat_w = first(weight)) %>% ungroup() %>%
        arrange(desc(cat_w), part, desc(abs(per_90))) %>% select(-cat_w)
    }
    rows <- rows %>% mutate(season = per_90 * s$minutes_played / 90)
    list(m = m, s = s, rows = rows, gap = rp$avg_minus_repl, repl = rp, total90 = total90,
         unit = if (m == "gar") "goals" else "points")
  })

  output$calc_sum <- renderUI({
    d <- calc_data(); s <- d$s
    term <- function(v, l, cls = "eq-term") div(class = cls, div(class = "v", v), div(class = "l", l))
    op <- function(x) span(class = "eq-op", x)
    per <- sprintf("%s per 90", toupper(d$m))
    parts <- if (d$m == "gar") list(
      term(fmt(sum(d$rows$per_90[d$rows$part == "attack"]), 3, TRUE), "attack vs average"), op("+"),
      term(fmt(sum(d$rows$per_90[d$rows$part == "defense"]), 3, TRUE), "defense vs average"), op("+"))
    else {
      # one term per category
      cs <- d$rows %>% group_by(part) %>% summarise(v = sum(per_90), .groups = "drop") %>%
        arrange(match(part, unique(d$rows$part)))
      lapply(seq_len(nrow(cs)), function(i) list(term(fmt(cs$v[i], 3, TRUE), tolower(cs$part[i])), op("+")))
    }
    div(class = "eq",
        parts,
        term(fmt(d$gap, 3, TRUE), sprintf("average %s vs replacement", s$position)), op("="),
        term(fmt(d$total90, 3, TRUE), per, "eq-term total"), op("×"),
        term(sprintf("%s / 90", format(s$minutes_played, big.mark = ",")), "minutes played"), op("="),
        term(fmt(if (d$m == "gar") s$gar else s$par, 2, TRUE),
             sprintf("%s this season (%s)", toupper(d$m), d$unit), "eq-term total"))
  })

  # Waterfall: each stat's credit per 90, then the replacement gap, ending at his rating per 90
  calc_items <- reactive({
    d <- calc_data()
    shown <- d$rows %>% filter(weight != 0)
    # PAR: one bar per category (its stats added together)
    if (d$m == "par") shown <- shown %>% group_by(label = part) %>% summarise(per_90 = sum(per_90), .groups = "drop") %>%
      arrange(match(label, unique(d$rows$part)))
    items <- bind_rows(
      shown %>% transmute(label, v = per_90, kind = if_else(per_90 >= 0, "up", "down")),
      tibble(label = sprintf("Average %s vs replacement level", d$s$position), v = d$gap, kind = if_else(d$gap >= 0, "up", "down"))) %>%
      mutate(end = cumsum(v), start = end - v)
    bind_rows(items, tibble(label = sprintf("%s per 90", toupper(d$m)), v = d$total90, kind = "total",
                            start = 0, end = d$total90)) %>%
      mutate(i = rev(seq_len(n())))
  })
  output$calc_plot <- renderPlot({
    it <- calc_items(); d <- calc_data()
    rng <- range(c(0, it$start, it$end)); pad <- diff(rng) * 0.18 + 1e-6
    ggplot(it) +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_segment(data = head(it, -1) %>% mutate(nxt = lead(i)) %>% filter(!is.na(nxt)),
                   aes(x = end, xend = end, y = i - 0.32, yend = nxt + 0.32), colour = "grey75", linewidth = 0.4) +
      geom_rect(aes(xmin = pmin(start, end), xmax = pmax(start, end), ymin = i - 0.32, ymax = i + 0.32, fill = kind)) +
      geom_text(aes(x = pmax(start, end), y = i, label = fmt_vec(v, 3, TRUE)), hjust = -0.15, size = 3.4) +
      scale_fill_manual(values = c(up = MAROON, down = GREY, total = SLATE), guide = "none") +
      scale_y_continuous(breaks = it$i, labels = it$label, expand = expansion(add = 0.5)) +
      scale_x_continuous(limits = c(min(rng[1], 0) - pad * 0.2, rng[2] + pad)) +
      labs(x = sprintf("%s per 90 (%s), vs an average %s unless noted", toupper(d$m), d$unit, d$s$position), y = NULL,
           caption = {
             zero <- if (d$m == "par") unique(d$rows$part[d$rows$weight == 0]) else d$rows$stat[d$rows$weight == 0]
             what <- if (d$m == "par") "categor" else "stat"
             if (length(zero)) sprintf("%d %s%s with weight 0 not shown (see the table)", length(zero), what,
                                       if (d$m == "par") (if (length(zero) > 1) "ies" else "y") else if (length(zero) > 1) "s" else "")
           }) +
      theme_board() +
      theme(panel.grid.major.y = element_blank(), plot.caption = element_text(colour = "#6B6B70", size = 9))
  }, height = function() 70 + 30 * nrow(calc_items()))

  output$calc_tbl <- renderDT({
    d <- calc_data()
    r <- d$rows
    tab <- if (d$m == "gar") r %>%
      transmute(Part = tools::toTitleCase(part), Stat = label,
                `His value` = raw, `After blending` = value, `Position avg` = pos_avg,
                Weight = weight, `vs avg / 90` = per_90, Season = season)
    else r %>%
      transmute(Category = part, Stat = if_else(coalesce(direction, 1) < 0, paste(label, "(fewer is better)"), label),
                `His value` = raw, `After blending` = value,
                `5th pct` = lo, `95th pct` = hi, `Scaled 0-1` = scaled, `Position avg (scaled)` = scaled_avg,
                `Share of category` = coalesce(share, 1), `Category weight` = weight,
                `vs avg / 90` = per_90, Season = season)
    tips <- c(`His value` = "His per-90 (or %) in conference games; a stat missing from the data counts as the position average",
              `After blending` = "Mixed with the position average as if he had also played a few hundred minutes at average level (small-sample shrinkage)",
              `Position avg` = "Minutes-weighted average of all players at his position this season, after blending",
              `5th pct` = "Value of the 5th-percentile player at his position this season (scaled to 0)",
              `95th pct` = "Value of the 95th-percentile player at his position this season (scaled to 1)",
              `Scaled 0-1` = "(After blending - 5th pct) / (95th pct - 5th pct); can go past 0 or 1. Flipped (1 - scaled) where fewer is better, so higher is always better",
              `Share of category` = "How much this stat counts inside its category (weight_in_category in par_categories.csv)",
              `Category weight` = "League points per 90 for each 1.0 of the category score (the 5th-to-95th percentile range); 0 = the data found no clear link",
              Weight = "Goals per 90 for each 1 unit of the stat (attack rows: share of the xG credited to him, times goals per xG)",
              `vs avg / 90` = if (d$m == "gar") "Weight x (his value - position average): his credit per 90 compared with an average player"
                              else "Category weight x share x (his scaled value - position average): his credit per 90 compared with an average player",
              Season = "vs avg / 90 x minutes / 90")
    head_tips <- htmltools::withTags(table(class = "display", thead(tr(lapply(names(tab), function(n)
      th(n, title = if (n %in% names(tips)) tips[[n]] else NULL))))))
    num <- setdiff(names(tab), c("Part", "Category", "Stat"))
    wcol <- intersect(c("Weight", "Category weight"), names(tab))
    # show each category's name once, on its first row
    if ("Category" %in% names(tab)) tab$Category[duplicated(tab$Category)] <- ""
    datatable(tab, rownames = FALSE, container = head_tips, class = "compact", selection = "none",
              options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-left", targets = which(!names(tab) %in% num) - 1)))) %>%
      formatRound(setdiff(num, c(wcol, "Share of category", "vs avg / 90", "Season")), 3) %>%
      formatRound(wcol, 4) %>% formatPercentage(intersect("Share of category", names(tab)), 0) %>%
      formatRound(c("vs avg / 90"), 3) %>% formatRound("Season", 2) %>%
      formatStyle(intersect("Category", names(tab)), fontWeight = "600", color = MAROON) %>%
      formatStyle(c("vs avg / 90", "Season"), color = styleInterval(0, c("#6B6B70", MAROON)), fontWeight = "600")
  })

  output$calc_steps <- renderUI({
    d <- calc_data(); s <- d$s; m <- d$m; r <- d$rows
    mins <- s$minutes_played
    prior <- as.numeric(setting(m, "PRIOR_MIN", 270)); min_q <- as.numeric(setting(m, "MIN_QUAL", 270))
    repl_p <- round(100 * as.numeric(setting(m, "REPL_PCTL", 0.2)))
    fit_s <- sub(", ([^,]+)$", " and \\1", setting(m, "FIT_SEASONS", ""))
    own <- round(100 * mins / (mins + prior))
    pos <- POS_LABELS[[s$position]]; pos_l <- tolower(pos)
    n_zero <- sum(r$weight == 0)
    f3 <- function(x) fmt(x, 3, TRUE)
    blend <- tags$li(tags$b("Small samples are pulled toward average. "),
      sprintf("Each per-90 stat is mixed with the %s average as if he had also played %d minutes at average level. With %s minutes, %d%% of each number is his own and %d%% is the average. This stops a hot few games from dominating.",
              pos_l, prior, format(mins, big.mark = ","), own, 100 - own),
      if (s$position == "GK" && m == "gar") " Goals prevented gets a much bigger pull (1,800 minutes), since a keeper's shot-stopping over a few games is mostly luck.")
    repl <- tags$li(tags$b("Replacement level. "),
      sprintf("A replacement-level %s is the %dth percentile of %ss with %d+ minutes this season: the kind of player who is easy to find. An average %s rates %s %s per 90 above that, and every %s gets this gap for his minutes, on top of how he compares with the average.",
              pos_l, repl_p, pos_l, min_q, pos_l, f3(d$gap), d$unit, pos_l))
    total <- tags$li(tags$b("Add it up. "),
      sprintf("%s per 90 = %s. Over %s minutes: %s x %s / 90 = %s %s.",
              toupper(m),
              # (attack) + (defense) + (gap): parentheses keep the signs readable
              paste(c(if (m == "gar") c(sprintf("(%s attack)", f3(sum(r$per_90[r$part == "attack"]))),
                                        sprintf("(%s defense)", f3(sum(r$per_90[r$part == "defense"]))))
                      else sprintf("(%s from his stats)", f3(sum(r$per_90))),
                      sprintf("(%s replacement gap)", f3(d$gap))), collapse = " + ") %>% paste("=", f3(d$total90)),
              format(mins, big.mark = ","), f3(d$total90), format(mins, big.mark = ","),
              fmt(if (m == "gar") s$gar else s$par, 2, TRUE), d$unit))
    if (m == "gar") {
      xg <- r %>% filter(stat == "xg"); xa <- r %>% filter(stat == "xa")
      pshare <- as.numeric(setting("gar", "PASSER_SHARE", 0.4)); xgv <- as.numeric(setting("gar", "XG_GOAL_VALUE", 1))
      keep <- if (nrow(xg)) xg$weight / xgv else NA
      assisted <- if (nrow(xg)) (1 - keep) / pshare else NA
      def <- r %>% filter(part == "defense", weight != 0) %>% arrange(desc(abs(per_90)))
      tags$ol(class = "steps",
        tags$li(tags$b("Start from his stats. "),
          sprintf("Everything comes from his per-90 numbers in conference games (%s, %d: %s minutes). The data is already adjusted for conference strength.",
                  s$team, s$season_year, format(mins, big.mark = ","))),
        blend,
        tags$li(tags$b("Attack: his share of his team's chances. "),
          if (nrow(xg)) sprintf(paste0("A team's chances are measured in non-penalty expected goals (npxG), and each one is split between the shooter and the passer. ",
                                      "The passer gets %d%% of the xG of every shot he sets up (his xA). The shooter keeps the rest: %d%% of %s's xG came from assisted shots, so he keeps %d%% of his own npxG. ",
                                      "Each npxG counts as %.2f goals, because college players scored %d%% of their non-penalty xG. ",
                                      "Shooting: %.3f npxG x %.2f = %.3f goals per 90 (average %s: %.3f, so %s). Passing: %.3f xA x %.2f = %.3f (average %.3f, so %s). Attack total: %s goals per 90 vs an average %s."),
                                round(100 * pshare), round(100 * assisted), s$team, round(100 * keep), xgv, round(100 * xgv),
                                xg$value, xg$weight, xg$credit, pos_l, xg$avg_credit, f3(xg$per_90),
                                xa$value, xa$weight, xa$credit, xa$avg_credit, f3(xa$per_90),
                                f3(sum(r$per_90[r$part == "attack"])), pos_l)),
        tags$li(tags$b("Defense: stats weighted by goals saved. "),
          if (!nrow(r[r$part == "defense", ])) sprintf("%ss have no defensive stats in the model, so their defense part is 0.", pos)
          else tagList(
            sprintf(paste0("Each defensive stat gets a weight in goals. The weights are learned from %s team-seasons: how many fewer goals a team concedes per game when its %ss are above average on that stat. ",
                           "Whether a stat helps or hurts is set in advance, and the data picks the size. "),
                    if (nzchar(fit_s)) paste0("the ", fit_s) else "all", pos_l),
            if (n_zero) sprintf("%d of his %d defensive stats got a weight of 0, meaning the data found no clear link to goals conceded. ", n_zero, sum(r$part == "defense")),
            if (nrow(def)) sprintf("Biggest part: %s, %.2f vs an average of %.2f, x %.4f = %s goals per 90. ",
                                   tolower(def$label[1]), def$value[1], def$pos_avg[1], def$weight[1], f3(def$per_90[1])),
            sprintf("Defense total: %s goals per 90 vs an average %s.", f3(sum(r$per_90[r$part == "defense"])), pos_l))),
        repl, total)
    } else {
      cats <- r %>% group_by(part) %>%
        summarise(w = first(weight), v = sum(per_90), stats = paste(tolower(label), collapse = ", "), .groups = "drop") %>%
        arrange(desc(w))
      top <- cats %>% filter(w != 0) %>% arrange(desc(abs(v)))
      zero_cats <- cats$part[cats$w == 0]
      massey <- identical(setting("par", "TARGET", "outcome"), "massey")
      ppm <- as.numeric(setting("par", "OUT_PER_TARGET", NA))
      tags$ol(class = "steps",
        tags$li(tags$b("Start from his stats. "),
          sprintf("PAR uses the stats listed for %ss in par_categories.csv, from conference games (%s, %d: %s minutes).",
                  pos_l, s$team, s$season_year, format(mins, big.mark = ","))),
        blend,
        tags$li(tags$b("Put every stat on the same 0-1 scale. "),
          sprintf("Within %ss this season, the 5th-percentile value counts as 0 and the 95th as 1. A player beyond them keeps going past 0 or 1. Stats where fewer is better (like cards) are flipped, so higher is always better. This makes a %% stat and a per-90 count comparable.", pos_l)),
        tags$li(tags$b("Group the stats into categories. "),
          sprintf("A %s has %d categories: %s. Each category score is the average of its stats on the 0-1 scale (some stats count more; see the Share column). Fewer weights than one per stat keeps them steadier.",
                  pos_l, nrow(cats), paste(sprintf("%s (%s)", tolower(cats$part), cats$stats), collapse = "; "))),
        tags$li(tags$b("Weight each category by what it's worth to a team. "),
          if (massey) sprintf(paste0("The weights are learned from %s team-seasons: how much higher a team's Massey rating is when its %ss are above average in each category. ",
                                     "Massey uses every game and the opponents, so it's steadier than a 5-12 game conference record. One Massey point is worth %.2f league points per game (3 for a win, 1 for a draw), which turns the weights into points. "),
                              if (nzchar(fit_s)) paste0("the ", fit_s) else "all", pos_l, ppm)
          else sprintf("The weights are learned from %s team-seasons: how many more points per game (3 for a win, 1 for a draw) a team earns when its %ss are above average in each category. ",
                       if (nzchar(fit_s)) paste0("the ", fit_s) else "all", pos_l),
          if (length(zero_cats)) sprintf("%s got a weight of 0: the data found no clear link once the other categories are counted. ",
                                         paste(zero_cats, collapse = " and ")),
          if (nrow(top)) sprintf("Biggest part: %s, %s points per 90 vs an average %s. Categories vs average total: %s points per 90.",
                                 tolower(top$part[1]), f3(top$v[1]), pos_l, f3(sum(r$per_90))),
          " The learned values for every position are in par_output_real/par_weights.csv; to set a category yourself, fill in fixed_points_per_season in par_categories.csv."),
        repl, total)
    }
  })

  # ---- History & info ------------------------------------------------------------
  output$hist <- renderPlot({
    s <- sel(); c <- comp()
    d <- bind_rows(players %>% filter(player_id == s$player_id) %>% mutate(who = "a"),
                   if (!is.null(c)) players %>% filter(player_id == c$player_id) %>% mutate(who = "b")) %>%
      mutate(who = factor(who, levels = c("a", "b")))
    ggplot(d, aes(x = factor(season_year), y = gar_per_90, fill = who)) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_col(position = position_dodge2(preserve = "single", padding = 0.15),
               width = min(0.7, 0.18 * n_distinct(d$season_year) + 0.2)) +
      geom_errorbar(aes(ymin = gar_per_90_lo, ymax = gar_per_90_hi),
                    position = position_dodge2(preserve = "single", padding = 0.75),
                    width = min(0.7, 0.18 * n_distinct(d$season_year) + 0.2), linewidth = 0.5) +
      scale_fill_manual(values = pcols(),
                        labels = c(a = s$player_name, b = if (!is.null(c)) c$player_name else ""), name = NULL,
                        drop = TRUE) +
      labs(x = NULL, y = "GAR per 90") +
      theme_board() +
      theme(panel.grid.major.x = element_blank())
  })

  # Every season of the selected player, with his rank at his position that season
  output$career_tbl <- renderDT({
    s <- sel()
    d <- players %>% filter(player_id == s$player_id) %>% arrange(desc(season_year)) %>%
      left_join(players %>% count(season_year, position, name = "n_pos"), by = c("season_year", "position")) %>%
      transmute(Season = season_year, Team = team, Pos = position, Min = minutes_played,
                GAR = gar, `GAR/90` = gar_per_90, PAR = par,
                `Rank at pos` = sprintf("#%d of %d", gar_rank_pos, n_pos))
    if (!HAS_PAR) d$PAR <- NULL
    datatable(d, rownames = FALSE, class = "compact", selection = "none",
              options = list(dom = "t", paging = FALSE, ordering = FALSE,
                             columnDefs = list(list(className = "dt-left", targets = 0:2)))) %>%
      formatRound("Min", 0) %>% formatRound("GAR", 2) %>% formatRound("GAR/90", 3) %>%
      formatRound(intersect("PAR", names(d)), 2) %>%
      formatStyle("Season", target = "row", fontWeight = styleEqual(s$season_year, "600"))
  })

  output$stats_tbl <- renderDT({
    s <- sel()
    tab <- stat_table(s$player_id, input$st_mode %||% "tot", s$position == "GK")
    validate(need(!is.null(tab) && nrow(tab) > 0, "No season stats for this player."))
    # show each group's name once, on its first row
    first <- !duplicated(tab$Group)
    tab$Group[!first] <- ""
    datatable(tab, rownames = FALSE, class = "compact", colnames = c("", "Stat", names(tab)[-(1:2)]),
              options = list(dom = "t", paging = FALSE, ordering = FALSE,
                             columnDefs = list(list(className = "dt-right", targets = 2:(ncol(tab) - 1))))) %>%
      formatStyle("Group", fontWeight = "600", color = MAROON)
  })

  output$facts <- renderUI({
    s <- sel()
    fact <- function(l, v) div(span(l), tags$b(v))
    div(class = "facts",
        fact("Conference", s$conference),
        fact("Team RPI", if (is.na(s$rpi_rank)) "—" else paste0("#", s$rpi_rank)),
        fact("Age", fmt(s$age, 1)),
        fact("Height", if (is.na(s$height_cm)) "—" else paste(round(s$height_cm), "cm")),
        fact("Foot", s$foot %||% "—"),
        if (HAS_CLASS) fact("Class", if (is.na(s$class_year)) "—" else CLASS_LABELS[s$class_year] %||% s$class_year),
        if (HAS_PORTAL) fact("Portal", if (is.na(s$portal_status)) "—" else
          paste0(s$portal_status, if (!is.na(s$portal_initiated)) paste0(" (", s$portal_initiated, ")") else "")),
        fact("Overall rank", paste0("#", format(s$gar_rank, big.mark = ","))),
        if (HAS_PAR) fact("PAR rank at position", if (is.na(s$par_rank_pos)) "—" else paste0("#", s$par_rank_pos)),
        if (HAS_PAR) fact("PAR per 90", fmt(s$par_per_90, 3)),
        fact("Attack / defense", sprintf("%s / %s", fmt(s$off_gaa, 2, TRUE), fmt(s$def_gaa, 2, TRUE))))
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("portal_board_%s.csv", input$season),
    content = function(file) write_csv(select(filtered(), -search, -key), file)
  )
}

fmt_vec <- function(x, d, sign = FALSE) formatC(x, format = "f", digits = d, flag = if (sign) "+" else "")

shinyApp(ui, server)
