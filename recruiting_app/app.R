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
# slashes, e.g. "C:/Users/you/Documents/project/GAR_output".
GAR_DIR       <- "../GAR_output"                      # output folder of player_gar.R
METRICS_FILE  <- "../simulated_player_metrics.csv"    # the Wyscout-style player file
FOCUS_TEAM    <- "Virginia Tech"
ROSTER_SEASON <- 2025      # season player_gar.R compares against (ROSTER_SEASON there)
PROJ_MINUTES  <- 1500      # season length behind "+Goals" (PROJ_MINUTES there)
COMPARE_MIN   <- 600       # minutes needed for the roster comparison (COMPARE_MIN there)
PCT_MIN       <- 600       # minutes needed to be in the percentile comparison group
DATA_NOTE     <- "Simulated data: names and numbers are made up"

# "real" = the Wyscout export (run prepare_real_data.R, then player_gar.R with
# DATA_SOURCE "real"); "simulated" = the simulated files and GAR_output
DATA_SOURCE   <- "real"
if (DATA_SOURCE == "real") {
  GAR_DIR       <- "../GAR_output_real"
  METRICS_FILE  <- "../real_player_metrics.csv"
  ROSTER_SEASON <- 2026
  COMPARE_MIN   <- 360     # one season so far, conference games only
  PCT_MIN       <- 270
  DATA_NOTE     <- "Wyscout data: conference games, 2026 season to date"
}
UPGRADE_MIN   <- 0.70      # "likely upgrade" filter: chance better than our weakest regular
GOALS_PER_WIN <- round(1 / 0.23, 1)

MAROON <- "#861F41"
ORANGE <- "#E5751F"
GREY   <- "#B9B3AE"
SLATE  <- "#44546A"   # the comparison player

# ---- Skill list: id, Wyscout column, label, group, lower is better ----------
SKILLS <- tribble(
  ~id, ~col, ~label, ~group, ~lower,
  "goals_90", "Goals / 90", "Goals per 90", "Shooting", FALSE,
  "npg_90", "Non-penalty goals, per 90 mins", "Non-penalty goals per 90", "Shooting", FALSE,
  "xg_90", "xG / 90", "Expected goals (xG) per 90", "Shooting", FALSE,
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

# ---- Load data once at startup ----------------------------------------------
# If a file isn't where the settings say, look for it in the app folder, the
# folder above, the one above that, and the current working directory
# (including their subfolders), and use the first match.
find_file <- function(path, label) {
  if (file.exists(path)) return(normalizePath(path))
  target <- basename(path)
  parent <- basename(dirname(path))          # e.g. "GAR_output" for the GAR files
  roots <- unique(normalizePath(c(".", "..", "../..", getwd()), mustWork = FALSE))
  for (r in roots) {
    hits <- list.files(r, pattern = paste0("^", gsub(".", "\\.", target, fixed = TRUE), "$"),
                       recursive = TRUE, full.names = TRUE)
    if (parent %in% c("GAR_output")) hits <- hits[basename(dirname(hits)) == parent]
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
portal <- read_csv(file.path(GAR_DIR, "portal_vs_roster.csv"), show_col_types = FALSE)
roster <- read_csv(file.path(GAR_DIR, "our_roster.csv"), show_col_types = FALSE)

missing_cols <- setdiff(SKILLS$col, names(read_csv(METRICS_FILE, n_max = 0, show_col_types = FALSE)))
if (length(missing_cols)) SKILLS <- filter(SKILLS, !col %in% missing_cols)

metrics <- read_csv(METRICS_FILE, show_col_types = FALSE,
                    col_select = any_of(c("season_year", "team", "player_id", "position",
                                          "age", "height_cm", "foot", "portal_status", "portal_initiated",
                                          "market_value", SKILLS$col)))
names(metrics) <- recode(names(metrics), !!!setNames(SKILLS$id, SKILLS$col))
metrics <- distinct(metrics, season_year, player_id, .keep_all = TRUE)

# Roster comparison for ROSTER_SEASON (portal file has no id, so join on name/team/position)
portal_join <- portal %>%
  transmute(player_name, team, position, season_year = ROSTER_SEASON,
            benchmark_player, benchmark_per_90, chance_better,
            season_upgrade, season_upgrade_lo, season_upgrade_hi)

players <- gar %>%
  left_join(metrics %>% select(season_year, player_id, age, height_cm, foot,
                               any_of(c("portal_status", "portal_initiated", "market_value"))),
            by = c("season_year", "player_id")) %>%
  left_join(portal_join, by = c("player_name", "team", "position", "season_year")) %>%
  mutate(search = tolower(paste(player_name, team, conference)))
for (nm in c("portal_status", "portal_initiated", "market_value"))
  if (!nm %in% names(players)) players[[nm]] <- NA
HAS_PORTAL <- any(!is.na(players$portal_status))
HAS_CLASS  <- any(!is.na(players$class_year))

# Our weakest regular at each position (the benchmark), with his id for skill lookups
benchmarks <- roster %>%
  filter(regular) %>%
  group_by(position) %>%
  slice_min(gar_per_90, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(gar %>% filter(season_year == ROSTER_SEASON, team == FOCUS_TEAM) %>%
              select(player_name, position, player_id),
            by = c("player_name", "position"))

SEASONS <- sort(unique(players$season_year), decreasing = TRUE)
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

# Our best regular at each position (a second built-in comparison)
best_regulars <- roster %>%
  filter(regular) %>%
  group_by(position) %>%
  slice_max(gar_per_90, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(gar %>% filter(season_year == ROSTER_SEASON, team == FOCUS_TEAM) %>%
              select(player_name, position, player_id),
            by = c("player_name", "position"))

players <- players %>% mutate(key = paste(season_year, player_id, sep = "|"))
Z80 <- qnorm(0.9)

theme_board <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = base),
          legend.position = "top", legend.justification = "left",
          legend.margin = margin(0, 0, 0, 0), legend.text = element_text(size = base - 1))
}

# ---- UI ----------------------------------------------------------------------
theme <- bs_theme(version = 5, primary = MAROON, secondary = ORANGE,
                  base_font = font_google("Inter", local = FALSE),
                  heading_font = font_google("Inter", local = FALSE))

ui <- page_sidebar(
  title = div(strong("Portal Recruiting Board"),
              span(class = "text-muted small ms-2", DATA_NOTE)),
  theme = theme,
  fillable = FALSE,
  tags$head(tags$style(HTML(sprintf("
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px rgba(134,31,65,.9) !important; color: #fff !important; }
    table.dataTable { font-size: .82rem; }
    table.dataTable th, table.dataTable td { white-space: nowrap; }
    table.dataTable tbody tr { cursor: pointer; }
    .who-name { font-size: 1.45rem; font-weight: 600; line-height: 1.15; margin: 0; }
    .who-sub { color: #6b6b6b; font-size: .88rem; margin-top: 2px; }
    .chip { display: inline-block; font-size: .72rem; font-weight: 600; padding: 2px 8px; border-radius: 999px;
            margin-left: 6px; vertical-align: 3px; }
    .chip-portal { background: #FCE9DC; color: #9A4A12; }
    .chip-ours { background: #F3E1E7; color: %s; }
    .kpis { display: flex; flex-wrap: wrap; gap: 28px; margin: 4px 0 2px; }
    .kpi-v { font-size: 1.5rem; font-weight: 600; line-height: 1.1; font-variant-numeric: tabular-nums; }
    .kpi-l { font-size: .75rem; color: #6b6b6b; }
    .cmp-row { display: flex; flex-wrap: wrap; gap: 16px; align-items: end; }
    .cmp-row .form-group { margin-bottom: 0; }
    .verdict { display: grid; gap: 10px; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); margin: 6px 0 4px; }
    .vd { background: #F7F5F3; border-radius: 8px; padding: 8px 12px; }
    .vd-v { font-size: 1.25rem; font-weight: 600; font-variant-numeric: tabular-nums; }
    .vd-l { font-size: .74rem; color: #6b6b6b; }
    .legend-dot { display: inline-block; width: 10px; height: 10px; border-radius: 50%%; margin: 0 4px 0 10px; vertical-align: -1px; }
    .note { font-size: .78rem; color: #6b6b6b; }
    .facts { display: grid; grid-template-columns: repeat(auto-fit, minmax(130px, 1fr)); gap: 8px 16px; font-size: .85rem; }
    .facts b { display: block; font-weight: 600; }
    .facts span { color: #6b6b6b; font-size: .75rem; }
  ", MAROON)))),
  sidebar = sidebar(
    width = 270,
    textInput("q", "Search", placeholder = "Name, team or conference"),
    selectInput("season", "Season", choices = SEASONS, selected = ROSTER_SEASON),
    selectInput("pos", "Position", choices = c("All positions" = "", setNames(names(POS_LABELS), POS_LABELS))),
    selectInput("conf", "Conference", choices = c("All conferences" = "", CONFS)),
    if (HAS_CLASS) selectInput("cls", "Class", choices = c("All classes" = "", setNames(names(CLASS_LABELS), CLASS_LABELS))),
    if (HAS_PORTAL) selectInput("portal", "Transfer portal",
                                choices = c("Anyone" = "", sort(unique(na.omit(players$portal_status))))),
    sliderInput("minmin", "Minimum minutes", min = 0, max = 2500, value = 300, step = 90),
    checkboxInput("upg", "Likely upgrades only"),
    div(class = "d-flex gap-2",
        actionButton("reset", "Reset", class = "btn-outline-secondary btn-sm"),
        downloadButton("dl", "Download", class = "btn-outline-secondary btn-sm"))
  ),
  
  layout_columns(
    col_widths = breakpoints(sm = 12, xl = c(5, 7)),
    card(
      full_screen = TRUE,
      card_header(textOutput("n_match", inline = TRUE)),
      DTOutput("tbl"),
      card_footer(class = "note",
                  sprintf("GAR = goals added over a replacement-level backup. Chance better = chance he rates above our weakest regular at his position (%d+ min).", COMPARE_MIN))
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
                    uiOutput("legend"),
                    plotOutput("gar_cmp", height = "170px"),
                    h6("Key skills (percentile at his position)", class = "mt-3 mb-0"),
                    plotOutput("radar", height = "380px")
          ),
          nav_panel("Skills",
                    uiOutput("legend2"),
                    div(class = "cmp-row",
                        radioButtons("skmode", NULL, inline = TRUE,
                                     choices = c("Key skills" = "key", "Biggest gaps" = "gaps", "All skills" = "all"))),
                    p(class = "note mb-1", sprintf(
                      "Percentile among same-season players at his position with %d+ minutes (higher is better; flipped where fewer is better). Hover a dot for the per-90 value.", PCT_MIN)),
                    plotOutput("dumbbell", height = "auto", hover = hoverOpts("db_hover", delay = 80)),
                    uiOutput("db_tip")
          ),
          nav_panel("History & info",
                    h6("GAR per 90 by season", class = "mb-0"),
                    plotOutput("hist", height = "240px"),
                    h6("Player info", class = "mt-3"),
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
    for (id in c("pos", "conf", "cls", "portal")) updateSelectInput(session, id, selected = "")
    updateSliderInput(session, "minmin", value = 300)
    updateCheckboxInput(session, "upg", value = FALSE)
  })
  
  q_slow <- debounce(reactive(input$q), 300)
  
  filtered <- reactive({
    d <- players %>% filter(season_year == as.numeric(input$season), minutes_played >= input$minmin)
    q <- trimws(tolower(q_slow() %||% ""))
    if (nzchar(q))                    d <- filter(d, grepl(q, search, fixed = TRUE))
    if (nzchar(input$pos))            d <- filter(d, position == input$pos)
    if (nzchar(input$conf))           d <- filter(d, conference == input$conf)
    if (nzchar(input$cls %||% ""))    d <- filter(d, class_year == input$cls)
    if (nzchar(input$portal %||% "")) d <- filter(d, portal_status == input$portal)
    if (isTRUE(input$upg))            d <- filter(d, !is.na(chance_better), chance_better >= UPGRADE_MIN)
    arrange(d, desc(gar))
  })
  
  output$n_match <- renderText({
    sprintf("%s players, %s season", format(nrow(filtered()), big.mark = ","), input$season)
  })
  
  output$tbl <- renderDT({
    d <- filtered() %>%
      transmute(Player = player_name, Team = team, Pos = position,
                Class = class_year, Portal = portal_status,
                Min = minutes_played, GAR = gar, `GAR/90` = gar_per_90,
                `Chance better` = chance_better)
    if (!HAS_CLASS)  d$Class  <- NULL
    if (!HAS_PORTAL) d$Portal <- NULL
    datatable(
      d, rownames = FALSE, selection = list(mode = "single", selected = 1),
      class = "compact hover nowrap",
      options = list(dom = "t", paging = FALSE, deferRender = TRUE, scrollY = "640px", scrollCollapse = TRUE,
                     scrollX = TRUE, order = list(list(which(names(d) == "GAR") - 1, "desc")),
                     columnDefs = list(list(className = "dt-left", targets = 0:(which(names(d) == "Min") - 2))))
    ) %>%
      formatRound("GAR", 2) %>%
      formatRound("GAR/90", 3) %>%
      formatRound("Min", 0) %>%
      formatPercentage("Chance better", 0) %>%
      formatStyle("GAR", background = styleColorBar(range(c(0, d$GAR), na.rm = TRUE), "#EAD3DA"),
                  backgroundSize = "95% 70%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # The selected player: the clicked row, or the top row of the list
  sel <- reactive({
    d <- filtered()
    req(nrow(d) > 0)
    i <- input$tbl_rows_selected
    if (length(i) == 0 || i > nrow(d)) i <- (input$tbl_rows_all %||% 1)[1]
    if (is.na(i) || i > nrow(d)) i <- 1
    d[i, ]
  })
  
  # ---- Comparison player ------------------------------------------------------
  observeEvent(input$season, {
    season_players <- players %>%
      filter(season_year == as.numeric(input$season) | (team == FOCUS_TEAM & season_year == ROSTER_SEASON)) %>%
      arrange(desc(team == FOCUS_TEAM), desc(gar))
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
  
  # ---- Header -------------------------------------------------------------------
  output$who <- renderUI({
    s <- sel()
    chips <- tagList(
      if (s$team == FOCUS_TEAM) span(class = "chip chip-ours", "Our player"),
      if (!is.na(s$portal_status)) span(class = "chip chip-portal", paste("Portal:", s$portal_status))
    )
    kpi <- function(v, l) div(div(class = "kpi-v", v), div(class = "kpi-l", l))
    tagList(
      div(class = "who-name", s$player_name, chips),
      div(class = "who-sub", sprintf("%s \u00b7 %s", POS_LABELS[[s$position]], s$team)),
      div(class = "kpis mt-2",
          kpi(fmt(s$gar), "GAR"),
          kpi(fmt(s$gar_per_90, 3), "GAR per 90"),
          kpi(paste0("#", s$gar_rank_pos), sprintf("of %s %ss", sum(players$season_year == s$season_year & players$position == s$position), s$position)),
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
    div(class = "verdict",
        vd(fmt(chance, pct = TRUE), sprintf("chance he's better than %s", c$player_name),
           if (chance >= 0.7) "#1E7B3A" else if (chance <= 0.3) "#B3261E" else "inherit"),
        vd(fmt(diff, 3, TRUE), "GAR per 90 gap"),
        vd(fmt(goals, 1, TRUE), sprintf("goals over a %s-min season (80%%: %s to %s)",
                                        format(PROJ_MINUTES, big.mark = ","), fmt(goals - half, 1, TRUE), fmt(goals + half, 1, TRUE))))
  })
  
  output_legend <- function() {
    s <- sel(); c <- comp()
    div(class = "note",
        span(class = "legend-dot", style = sprintf("background:%s;margin-left:0", MAROON)), s$player_name,
        if (!is.null(c)) tagList(span(class = "legend-dot", style = sprintf("background:%s", SLATE)),
                                 sprintf("%s (%s)", c$player_name, c$role)))
  }
  output$legend <- renderUI(output_legend())
  
  output$legend2 <- renderUI(output_legend())
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
      scale_fill_manual(values = c(Attack = MAROON, Defense = ORANGE, Replacement = GREY),
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
      scale_fill_manual(values = c(a = MAROON, b = SLATE), guide = "none") +
      scale_colour_manual(values = c(a = MAROON, b = SLATE), guide = "none") +
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
      geom_point(aes(x = pct_b), colour = SLATE, size = 3.4, na.rm = TRUE) +
      geom_point(aes(x = pct_a), colour = MAROON, size = 3.6) +
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
      scale_fill_manual(values = c(a = MAROON, b = SLATE),
                        labels = c(a = s$player_name, b = if (!is.null(c)) c$player_name else ""), name = NULL,
                        drop = TRUE) +
      labs(x = NULL, y = "GAR per 90") +
      theme_board() +
      theme(panel.grid.major.x = element_blank())
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
        fact("Wins above replacement", fmt(s$war_equiv)),
        fact("Attack / defense", sprintf("%s / %s", fmt(s$off_gaa, 2, TRUE), fmt(s$def_gaa, 2, TRUE))))
  })
  
  output$dl <- downloadHandler(
    filename = function() sprintf("portal_board_%s.csv", input$season),
    content = function(file) write_csv(select(filtered(), -search, -key), file)
  )
}

fmt_vec <- function(x, d) formatC(x, format = "f", digits = d)

shinyApp(ui, server)