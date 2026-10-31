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

# ---- Settings (paths are relative to this app's folder) ---------------------
GAR_DIR       <- "../VTSoccer/GAR_output"                      # output folder of player_gar.R
METRICS_FILE  <- "../VTSoccer/simulated_player_metrics.csv"    # the Wyscout-style player file
FOCUS_TEAM    <- "Virginia Tech"
ROSTER_SEASON <- 2025      # season player_gar.R compares against (ROSTER_SEASON there)
PROJ_MINUTES  <- 1500      # season length behind "+Goals" (PROJ_MINUTES there)
COMPARE_MIN   <- 600       # minutes needed for the roster comparison (COMPARE_MIN there)
PCT_MIN       <- 600       # minutes needed to be in the percentile comparison group
UPGRADE_MIN   <- 0.70      # "likely upgrade" filter: chance better than our weakest regular
GOALS_PER_WIN <- round(1 / 0.23, 1)

MAROON <- "#861F41"
ORANGE <- "#E5751F"
GREY   <- "#B9B3AE"

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
  "exits_90", "Exits, per 90 mins", "Exits per 90", "Goalkeeping", FALSE
)
GROUP_ORDER <- unique(SKILLS$group)
GK_GROUPS   <- c("Passing", "Aerial", "Security (fewer better)", "Goalkeeping")

POS_LABELS <- c(GK = "Goalkeeper", CB = "Center back", FB = "Fullback",
                DM = "Defensive mid", CM = "Central mid", AM = "Attacking mid",
                W = "Winger", CF = "Center forward")
CLASS_LABELS <- c(Fr = "Freshman", So = "Sophomore", Jr = "Junior", Sr = "Senior", Gr = "Graduate")

# ---- Load data once at startup ----------------------------------------------
stop_if_missing <- function(f) if (!file.exists(f))
  stop("Can't find ", normalizePath(f, mustWork = FALSE),
       ". Check the paths at the top of app.R (they are relative to the app folder).")
for (f in c(file.path(GAR_DIR, c("player_gar.csv", "portal_vs_roster.csv", "our_roster.csv")), METRICS_FILE))
  stop_if_missing(f)

gar <- read_csv(file.path(GAR_DIR, "player_gar.csv"), show_col_types = FALSE)
portal <- read_csv(file.path(GAR_DIR, "portal_vs_roster.csv"), show_col_types = FALSE)
roster <- read_csv(file.path(GAR_DIR, "our_roster.csv"), show_col_types = FALSE)

missing_cols <- setdiff(SKILLS$col, names(read_csv(METRICS_FILE, n_max = 0, show_col_types = FALSE)))
if (length(missing_cols)) SKILLS <- filter(SKILLS, !col %in% missing_cols)

metrics <- read_csv(METRICS_FILE, show_col_types = FALSE,
                    col_select = any_of(c("season_year", "team", "player_id", "position",
                                          "age", "height_cm", "foot", SKILLS$col)))
names(metrics) <- recode(names(metrics), !!!setNames(SKILLS$id, SKILLS$col))
metrics <- distinct(metrics, season_year, player_id, .keep_all = TRUE)

# Roster comparison for ROSTER_SEASON (portal file has no id, so join on name/team/position)
portal_join <- portal %>%
  transmute(player_name, team, position, season_year = ROSTER_SEASON,
            benchmark_player, benchmark_per_90, chance_better,
            season_upgrade, season_upgrade_lo, season_upgrade_hi)

players <- gar %>%
  left_join(metrics %>% select(season_year, player_id, age, height_cm, foot),
            by = c("season_year", "player_id")) %>%
  left_join(portal_join, by = c("player_name", "team", "position", "season_year")) %>%
  mutate(search = tolower(paste(player_name, team, conference)))

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

# ---- UI ----------------------------------------------------------------------
theme <- bs_theme(version = 5, primary = MAROON, secondary = ORANGE,
                  base_font = font_google("Inter", local = FALSE),
                  heading_font = font_google("Inter", local = FALSE))

ui <- page_sidebar(
  title = div(strong("Portal Recruiting Board"),
              span(class = "text-muted small ms-2", "Simulated data: names and numbers are made up")),
  theme = theme,
  fillable = FALSE,
  tags$head(tags$style(HTML(sprintf("
    .sk-grid { display: grid; gap: 14px; grid-template-columns: repeat(auto-fit, minmax(340px, 1fr)); }
    .sk-h { margin: 0 0 4px; font-weight: 600; color: %s; }
    .sk-row { display: grid; grid-template-columns: minmax(0, 1.5fr) 4.2rem minmax(6rem, 1fr); gap: 8px;
              align-items: center; font-size: .82rem; padding: 3px 0; border-bottom: 1px solid #eee; }
    .sk-v { text-align: right; font-variant-numeric: tabular-nums; }
    .sk-bar { position: relative; height: 16px; }
    .sk-track { position: absolute; left: 0; right: 2.2rem; top: 5px; height: 7px; border-radius: 4px; background: #EEEAE6; }
    .sk-fill { position: absolute; left: 0; top: 0; bottom: 0; border-radius: 4px; background: %s; }
    .sk-tick { position: absolute; top: -4px; width: 2px; height: 15px; margin-left: -1px; background: #111; }
    .sk-n { position: absolute; right: 0; top: -1px; width: 2rem; text-align: right; color: #666; font-variant-numeric: tabular-nums; }
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px rgba(134,31,65,.9) !important; color: #fff !important; }
    table.dataTable { font-size: .82rem; }
    table.dataTable th, table.dataTable td { white-space: nowrap; }
    table.dataTable tbody tr { cursor: pointer; }
    .pr-tiles { display: grid; gap: 10px; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); }
    .pr-tile { border: 1px solid #e5e1dd; border-radius: 8px; padding: 10px 12px; }
    .pr-tile-l { font-size: .78rem; color: #666; }
    .pr-big { font-size: 1.7rem; font-weight: 600; line-height: 1.2; font-variant-numeric: tabular-nums; color: %s; }
    .pr-tile-s { font-size: .75rem; color: #666; font-variant-numeric: tabular-nums; }
  ", MAROON, MAROON, MAROON)))),
  sidebar = sidebar(
    width = 290,
    textInput("q", "Search name, team or conference", placeholder = "e.g. Furman"),
    selectInput("season", "Season", choices = SEASONS, selected = ROSTER_SEASON),
    selectInput("pos", "Position", choices = c("All positions" = "", setNames(names(POS_LABELS), POS_LABELS))),
    selectInput("conf", "Conference", choices = c("All conferences" = "", CONFS)),
    selectInput("cls", "Class", choices = c("All classes" = "", setNames(names(CLASS_LABELS), CLASS_LABELS))),
    sliderInput("minmin", "Minimum minutes", min = 0, max = 2500, value = 300, step = 90),
    checkboxInput("upg", sprintf("Likely upgrades only (%d%%+ chance better than our weakest regular)",
                                 round(100 * UPGRADE_MIN))),
    actionButton("reset", "Reset filters", class = "btn-outline-secondary btn-sm"),
    hr(),
    downloadButton("dl", "Download filtered list", class = "btn-sm"),
    p(class = "small text-muted mt-3",
      "GAR = goals added over a replacement-level backup at his position. ",
      sprintf("'Chance better' and '+Goals' compare with our weakest regular at the position (%d roster; %d+ minutes; +Goals over a %s-minute season).",
              ROSTER_SEASON, COMPARE_MIN, format(PROJ_MINUTES, big.mark = ",")))
  ),

  layout_columns(
    col_widths = breakpoints(sm = 12, xl = c(6, 6)),
    card(
      full_screen = TRUE,
      card_header(textOutput("n_match", inline = TRUE)),
      DTOutput("tbl")
    ),
    card(
      full_screen = TRUE,
      card_header(uiOutput("who")),
      card_body(
        uiOutput("tiles"),
        h6("Where his GAR comes from", class = "mt-3"),
        plotOutput("parts", height = "110px"),
        uiOutput("vt"),
        h6("GAR per 90 by season", class = "mt-3"),
        plotOutput("hist", height = "210px"),
        h6("Skills", class = "mt-3"),
        p(class = "small text-muted",
          sprintf("Bar = percentile among %s players at his position with %d+ minutes (higher is better; flipped where fewer is better). Number = his per-90 value or %%. Black tick = our weakest regular at the position.",
                  "same-season", PCT_MIN)),
        uiOutput("skills")
      )
    )
  )
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateTextInput(session, "q", value = "")
    updateSelectInput(session, "season", selected = ROSTER_SEASON)
    for (id in c("pos", "conf", "cls")) updateSelectInput(session, id, selected = "")
    updateSliderInput(session, "minmin", value = 300)
    updateCheckboxInput(session, "upg", value = FALSE)
  })

  q_slow <- debounce(reactive(input$q), 300)

  filtered <- reactive({
    d <- players %>% filter(season_year == as.numeric(input$season), minutes_played >= input$minmin)
    q <- trimws(tolower(q_slow() %||% ""))
    if (nzchar(q))          d <- filter(d, grepl(q, search, fixed = TRUE))
    if (nzchar(input$pos))  d <- filter(d, position == input$pos)
    if (nzchar(input$conf)) d <- filter(d, conference == input$conf)
    if (nzchar(input$cls))  d <- filter(d, class_year == input$cls)
    if (isTRUE(input$upg))  d <- filter(d, !is.na(chance_better), chance_better >= UPGRADE_MIN)
    arrange(d, desc(gar))
  })

  output$n_match <- renderText({
    sprintf("%s players match, %s season. Click a row to open his profile.",
            format(nrow(filtered()), big.mark = ","), input$season)
  })

  output$tbl <- renderDT({
    d <- filtered() %>%
      transmute(Player = player_name, Team = team, Pos = position, Class = class_year,
                Min = minutes_played, GAR = gar, `GAR/90` = gar_per_90,
                Attack = off_gaa, Defense = def_gaa,
                `Chance better` = chance_better, `+Goals` = season_upgrade)
    datatable(
      d, rownames = FALSE, selection = list(mode = "single", selected = 1),
      class = "compact hover nowrap",
      options = list(dom = "t", paging = FALSE, deferRender = TRUE, scrollY = "620px", scrollCollapse = TRUE,
                     scrollX = TRUE, order = list(list(5, "desc")),
                     columnDefs = list(list(className = "dt-left", targets = 0:3)))
    ) %>%
      formatRound(c("GAR", "Attack", "Defense", "+Goals"), 2) %>%
      formatRound("GAR/90", 3) %>%
      formatRound("Min", 0) %>%
      formatPercentage("Chance better", 0) %>%
      formatStyle("+Goals", color = styleInterval(0, c("#B3261E", "#1E7B3A"))) %>%
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

  # Skills for the selected player + percentiles within season/position (600+ min)
  skill_tab <- reactive({
    s <- sel()
    ref <- metrics %>%
      semi_join(gar %>% filter(minutes_played >= PCT_MIN), by = c("season_year", "player_id")) %>%
      filter(season_year == s$season_year, position == s$position)
    me <- metrics %>% filter(season_year == s$season_year, player_id == s$player_id)
    bm_id <- benchmarks$player_id[benchmarks$position == s$position]
    bm <- metrics %>% filter(season_year == ROSTER_SEASON, player_id %in% bm_id)
    groups <- if (s$position == "GK") GK_GROUPS else setdiff(GROUP_ORDER, "Goalkeeping")
    SKILLS %>%
      filter(group %in% groups) %>%
      rowwise() %>%
      mutate(value = if (nrow(me)) me[[id]][1] else NA_real_,
             pct   = pct_in(value, ref[[id]], lower),
             bm_pct = if (nrow(bm)) pct_in(bm[[id]][1], ref[[id]], lower) else NA_real_) %>%
      ungroup() %>%
      filter(!is.na(value))
  })

  output$who <- renderUI({
    s <- sel()
    tagList(
      h4(s$player_name, class = "mb-0"),
      div(class = "text-muted small",
          sprintf("%s · %s · %s · %s · %s season", s$team, s$conference,
                  POS_LABELS[[s$position]], CLASS_LABELS[[s$class_year]] %||% s$class_year, s$season_year)),
      div(class = "small mt-1",
          sprintf("Age %s · Height %s cm · %s foot · %s minutes · Team RPI #%s",
                  fmt(s$age, 1), fmt(s$height_cm, 0), s$foot %||% "—",
                  format(s$minutes_played, big.mark = ","), s$rpi_rank))
    )
  })

  output$tiles <- renderUI({
    s <- sel()
    tile <- function(title, value, sub) div(class = "pr-tile",
      div(class = "pr-tile-l", title), div(class = "pr-big", value), div(class = "pr-tile-s", sub))
    div(class = "pr-tiles",
      tile("GAR", fmt(s$gar), sprintf("80%% range %s to %s", fmt(s$gar_lo), fmt(s$gar_hi))),
      tile("GAR per 90", fmt(s$gar_per_90, 3), sprintf("80%% range %s to %s", fmt(s$gar_per_90_lo, 3), fmt(s$gar_per_90_hi, 3))),
      tile("Rank at position", paste0("#", s$gar_rank_pos), sprintf("#%s overall", format(s$gar_rank, big.mark = ","))),
      tile("In wins", fmt(s$war_equiv), sprintf("about %s goals per win", GOALS_PER_WIN))
    )
  })

  output$parts <- renderPlot({
    s <- sel()
    d <- tibble(part = factor(c("Attack", "Defense", "Replacement"), levels = c("Replacement", "Defense", "Attack")),
                v = c(s$off_gaa, s$def_gaa, s$repl_value), y = "GAR")
    ggplot(d, aes(x = v, y = y)) +
      geom_col(aes(fill = part), width = 0.5) +
      geom_vline(xintercept = 0, colour = "grey40") +
      geom_errorbarh(aes(xmin = s$gar_lo, xmax = s$gar_hi, y = 1), height = 0.25, linewidth = 0.7,
                     inherit.aes = FALSE, data = tibble(x = 1)) +
      geom_point(aes(x = s$gar, y = 1), shape = 21, size = 3.5, fill = "white", stroke = 1,
                 inherit.aes = FALSE, data = tibble(x = 1)) +
      scale_fill_manual(values = c(Attack = MAROON, Defense = ORANGE, Replacement = GREY),
                        breaks = c("Attack", "Defense", "Replacement"),
                        labels = c(sprintf("Attack %s", fmt(s$off_gaa, 2, TRUE)),
                                   sprintf("Defense %s", fmt(s$def_gaa, 2, TRUE)),
                                   sprintf("Average over replacement %s", fmt(s$repl_value, 2, TRUE)))) +
      labs(x = "Goals above replacement (dot = total, line = 80% range)", y = NULL, fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(), legend.margin = margin(0, 0, 0, 0),
            legend.text = element_text(size = 11))
  })

  output$vt <- renderUI({
    s <- sel()
    body <- if (s$team == FOCUS_TEAM) {
      p("He is one of our players.")
    } else if (s$season_year != ROSTER_SEASON) {
      p(class = "text-muted", sprintf("The roster comparison is for the %d season; switch the season filter to %d to see it.", ROSTER_SEASON, ROSTER_SEASON))
    } else if (is.na(s$chance_better)) {
      p(class = "text-muted", sprintf("He has fewer than %d minutes, so there is no roster comparison.", COMPARE_MIN))
    } else {
      tagList(
        p(HTML(sprintf("Chance he rates above our weakest regular at %s, <b>%s</b> (%s GAR/90): <b>%s</b>.",
                       s$position, htmltools::htmlEscape(s$benchmark_player), fmt(s$benchmark_per_90, 3), fmt(s$chance_better, pct = TRUE)))),
        p(HTML(sprintf("Over a %s-minute season he would add <b>%s</b> goals over him (80%% range %s to %s), about %s wins.",
                       format(PROJ_MINUTES, big.mark = ","), fmt(s$season_upgrade, 1, TRUE),
                       fmt(s$season_upgrade_lo, 1, TRUE), fmt(s$season_upgrade_hi, 1, TRUE),
                       fmt(s$season_upgrade / GOALS_PER_WIN, 2, TRUE))))
      )
    }
    div(class = "mt-3", h6(sprintf("Compared with our roster (%s)", FOCUS_TEAM)), body)
  })

  output$hist <- renderPlot({
    s <- sel()
    d <- players %>% filter(player_id == s$player_id) %>% arrange(season_year) %>%
      mutate(lab = sprintf("%s\n%s min", team, format(minutes_played, big.mark = ",")),
             now = season_year == s$season_year)
    ggplot(d, aes(x = factor(season_year), y = gar_per_90)) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_col(aes(fill = now), width = 0.55) +
      geom_errorbar(aes(ymin = gar_per_90_lo, ymax = gar_per_90_hi), width = 0.15, linewidth = 0.6) +
      geom_text(aes(y = pmin(0, gar_per_90_lo), label = lab), vjust = 1.3, size = 3.6,
                lineheight = 0.9, colour = "grey30") +
      scale_fill_manual(values = c(`TRUE` = MAROON, `FALSE` = GREY), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0.45, 0.08))) +
      labs(x = NULL, y = "GAR per 90") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
  })

  output$skills <- renderUI({
    d <- skill_tab()
    validate(need(nrow(d) > 0, "No skill data for this player."))
    d <- d %>% mutate(shown = ifelse(grepl("_pct$", id), sprintf("%.1f%%", value),
                                     ifelse(abs(value) < 1, sprintf("%.3f", value), sprintf("%.2f", value))))
    row_ui <- function(r) {
      op <- if (is.na(r$pct)) 0 else if (r$pct >= 80) 1 else if (r$pct >= 50) 0.7 else 0.45
      div(class = "sk-row",
          span(class = "sk-l", r$label),
          span(class = "sk-v", r$shown),
          span(class = "sk-bar",
               span(class = "sk-track",
                    span(class = "sk-fill", style = sprintf("width:%.0f%%;opacity:%.2f", ifelse(is.na(r$pct), 0, r$pct), op)),
                    if (!is.na(r$bm_pct)) span(class = "sk-tick", style = sprintf("left:%.0f%%", r$bm_pct),
                                               title = sprintf("Our weakest regular: %.0f", r$bm_pct))),
               span(class = "sk-n", ifelse(is.na(r$pct), "—", round(r$pct)))))
    }
    groups <- intersect(GROUP_ORDER, d$group)
    div(class = "sk-grid",
        lapply(groups, function(g) {
          gd <- d[d$group == g, ]
          div(class = "sk-grp", h6(class = "sk-h", g),
              lapply(seq_len(nrow(gd)), function(i) row_ui(gd[i, ])))
        }))
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("portal_board_%s.csv", input$season),
    content = function(file) write_csv(select(filtered(), -search), file)
  )
}

shinyApp(ui, server)
