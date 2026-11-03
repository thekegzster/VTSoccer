# =============================================================================
# position_gaps.R: where does Virginia Tech stand, position by position, against
# the average top-10 and top-30 team? Uses both GAR and PAR.
#
# For every team and position it measures:
#   quality       = minutes-weighted GAR (or PAR) per 90 of the players who played
#                   there: how good the minutes at that position were
#   contribution  = total GAR (or PAR) from that position per game: quality x how
#                   much the position is used (formations with 3 CBs score higher here)
# Then compares Virginia Tech with the average top-10 and top-30 team, and ranks
# the positions to improve. Gaps are also put on one scale (team-to-team standard
# deviations at the position) so GAR and PAR can be averaged into one priority.
#
# Run AFTER player_gar.R and player_war.R (OUTCOME <- "points").
#   Rscript position_gaps.R
# Outputs (OUT_DIR): position_gaps.csv, priorities.csv, vt_players_by_position.csv, charts
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---- Settings ---------------------------------------------------------------
DATA_SOURCE <- "real"            # "real" or "simulated"
FOCUS_TEAM  <- "Virginia Tech"
RANK_BY     <- "massey"             # how "top" teams are picked: "rpi" (RPI rank) or "massey" (Massey rating, real data)
TOP_A       <- 10                # first comparison group: top 10
TOP_B       <- 30                # second comparison group: top 30 (ranks 1-30, so it includes the top 10)
SEASON      <- NA                # NA = latest season in the data
MAKE_PLOTS  <- TRUE

if (DATA_SOURCE == "real") {
  GAR_DIR <- "GAR_output_real"; PAR_DIR <- "par_output_real"
  STANDINGS_FILE <- "real_standings.csv"; OUT_DIR <- "position_gaps_real"
} else {
  GAR_DIR <- "GAR_output"; PAR_DIR <- "par_output"
  STANDINGS_FILE <- "simulated_standings.csv"; OUT_DIR <- "position_gaps"
}
POS <- c("GK", "CB", "FB", "DM", "CM", "AM", "W", "CF")
POS_NAMES <- c(GK = "Goalkeeper", CB = "Center back", FB = "Fullback", DM = "Defensive mid",
               CM = "Central mid", AM = "Attacking mid", W = "Winger", CF = "Center forward")
MAROON <- "#861F41"; ORANGE <- "#E5751F"; SLATE <- "#5B6B82"; GREY <- "#C9C3BD"
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- Load -------------------------------------------------------------------
for (f in c(file.path(GAR_DIR, "player_gar.csv"), file.path(PAR_DIR, "player_par.csv"), STANDINGS_FILE))
  if (!file.exists(f)) stop("Missing ", f, ". Run player_gar.R and player_war.R (OUTCOME <- \"points\") first.")
gar <- read_csv(file.path(GAR_DIR, "player_gar.csv"), show_col_types = FALSE)
par <- read_csv(file.path(PAR_DIR, "player_par.csv"), show_col_types = FALSE)
stand <- read_csv(STANDINGS_FILE, show_col_types = FALSE)
if (is.na(SEASON)) SEASON <- max(gar$season_year)

teams <- stand %>% filter(season_year == SEASON) %>%
  mutate(games = if ("games" %in% names(.)) games else overall_w + overall_l + overall_t)
teams$rank <- if (RANK_BY == "massey" && "team_rating" %in% names(teams)) {
  rank(-teams$team_rating, ties.method = "min")
} else teams$rpi_rank
teams <- teams %>% mutate(group = case_when(team == FOCUS_TEAM ~ "VT",
                                            rank <= TOP_A ~ "topA", rank <= TOP_B ~ "topB", TRUE ~ "rest"))
if (!FOCUS_TEAM %in% teams$team) stop(FOCUS_TEAM, " is not in ", STANDINGS_FILE)
LAB_A <- sprintf("Top %d avg", TOP_A); LAB_B <- sprintf("Top %d avg", TOP_B)
rank_name <- if (RANK_BY == "massey") "Massey rating" else "RPI"

players <- gar %>% filter(season_year == SEASON) %>%
  select(team, player_id, player_name, position, minutes_played, gar, gar_per_90) %>%
  left_join(par %>% filter(season_year == SEASON) %>% select(player_id, par, par_per_90), by = "player_id") %>%
  filter(minutes_played > 0)

# ---- Team x position measures ----------------------------------------------
tp <- players %>%
  group_by(team, position) %>%
  summarise(minutes = sum(minutes_played),
            gar_q = weighted.mean(gar_per_90, minutes_played),
            par_q = weighted.mean(par_per_90, minutes_played, na.rm = TRUE),
            gar_tot = sum(gar), par_tot = sum(par, na.rm = TRUE), .groups = "drop") %>%
  complete(team, position = POS, fill = list(minutes = 0, gar_tot = 0, par_tot = 0)) %>%
  inner_join(teams %>% select(team, games, rank, group), by = "team") %>%
  mutate(gar_pg = gar_tot / games, par_pg = par_tot / games,
         share = minutes / (games * 990))    # share of the team's minutes at this position

long <- tp %>%
  select(team, position, rank, group, share, gar_q, par_q, gar_pg, par_pg) %>%
  pivot_longer(c(gar_q, par_q, gar_pg, par_pg), names_to = "key", values_to = "value") %>%
  mutate(model = toupper(sub("_.*", "", key)),
         measure = if_else(grepl("_q$", key), "quality", "contribution"))

# Averages for the groups (top 30 = ranks 1-30, top 10 included); SD across all teams
summ <- long %>%
  group_by(position, model, measure) %>%
  summarise(vt = value[group == "VT"][1],
            top_a = mean(value[rank <= TOP_A], na.rm = TRUE),
            top_b = mean(value[rank <= TOP_B], na.rm = TRUE),
            all_avg = mean(value, na.rm = TRUE),
            sd_teams = sd(value, na.rm = TRUE),
            vt_rank = rank(-value, ties.method = "min", na.last = "keep")[group == "VT"][1],
            n_teams = sum(!is.na(value)), .groups = "drop") %>%
  mutate(gap_a = vt - top_a, gap_b = vt - top_b,
         gap_a_sd = gap_a / sd_teams, gap_b_sd = gap_b / sd_teams,
         position = factor(position, levels = POS)) %>%
  arrange(measure, model, position)
write_csv(summ %>% mutate(across(where(is.double), ~ round(.x, 4))), file.path(OUT_DIR, "position_gaps.csv"))

# Priority: average of the GAR and PAR quality gaps to the top 10, in SD units
pri <- summ %>% filter(measure == "quality") %>%
  select(position, model, gap_a_sd, gap_b_sd, vt_rank, n_teams) %>%
  pivot_wider(names_from = model, values_from = c(gap_a_sd, gap_b_sd, vt_rank)) %>%
  mutate(gap_top_a = (gap_a_sd_GAR + gap_a_sd_PAR) / 2, gap_top_b = (gap_b_sd_GAR + gap_b_sd_PAR) / 2,
         models_agree = sign(gap_a_sd_GAR) == sign(gap_a_sd_PAR)) %>%
  arrange(gap_top_a) %>%
  mutate(priority = row_number())
write_csv(pri %>% mutate(across(where(is.double), ~ round(.x, 3))), file.path(OUT_DIR, "priorities.csv"))

vt_players <- players %>% filter(team == FOCUS_TEAM) %>%
  mutate(position = factor(position, levels = POS)) %>%
  arrange(position, desc(minutes_played)) %>%
  transmute(position, player_name, minutes_played, gar = round(gar, 2), gar_per_90 = round(gar_per_90, 3),
            par = round(par, 2), par_per_90 = round(par_per_90, 3))
write_csv(vt_players, file.path(OUT_DIR, "vt_players_by_position.csv"))

# ---- Report -----------------------------------------------------------------
vt_row <- teams %>% filter(team == FOCUS_TEAM)
cat(sprintf("%s, %d season: %s #%d, %.1f games in the data. Top %d and top %d by %s.\n\n",
            FOCUS_TEAM, SEASON, rank_name, vt_row$rank, vt_row$games, TOP_A, TOP_B, rank_name))
show <- function(meas, mdl, digits) {
  d <- summ %>% filter(measure == meas, model == mdl) %>%
    transmute(Position = as.character(position), VT = vt, !!LAB_A := top_a, !!LAB_B := top_b,
              `Gap to top A` = gap_a, `VT rank` = sprintf("%d of %d", vt_rank, n_teams))
  names(d)[names(d) == "Gap to top A"] <- sprintf("Gap to top %d", TOP_A)
  print(as.data.frame(d %>% mutate(across(where(is.double), ~ round(.x, digits)))), row.names = FALSE)
}
cat("QUALITY: minutes-weighted GAR per 90 at each position\n"); show("quality", "GAR", 3)
cat("\nQUALITY: minutes-weighted PAR per 90 at each position\n"); show("quality", "PAR", 3)
cat("\nCONTRIBUTION: total GAR per game from each position\n"); show("contribution", "GAR", 3)
cat("\nCONTRIBUTION: total PAR per game from each position\n"); show("contribution", "PAR", 3)
cat(sprintf("\nPRIORITIES (quality gap to the top %d, in team-to-team standard deviations; negative = behind)\n", TOP_A))
print(as.data.frame(pri %>% transmute(priority, position = as.character(position),
                                      GAR = round(gap_a_sd_GAR, 2), PAR = round(gap_a_sd_PAR, 2),
                                      average = round(gap_top_a, 2), models_agree)), row.names = FALSE)

# ---- Charts -----------------------------------------------------------------
if (MAKE_PLOTS) {
  theme_g <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold"), plot.title.position = "plot",
          legend.position = "top", legend.justification = "left", strip.text = element_text(face = "bold", size = 12))
  pos_lab <- function(x) factor(POS_NAMES[as.character(x)], levels = rev(POS_NAMES[POS]))
  marks <- c("VT" = ORANGE, "Top A" = MAROON, "Top B" = SLATE)
  names(marks) <- c(FOCUS_TEAM, LAB_A, LAB_B)

  chart_group <- function(meas, title, xlab, file) {
    pts <- long %>% filter(measure == meas, rank <= TOP_B, group != "VT") %>% mutate(pos = pos_lab(position))
    avg <- summ %>% filter(measure == meas) %>%
      select(position, model, vt, top_a, top_b) %>%
      pivot_longer(c(vt, top_a, top_b), names_to = "who", values_to = "value") %>%
      mutate(who = factor(recode(who, vt = FOCUS_TEAM, top_a = LAB_A, top_b = LAB_B), levels = names(marks)),
             pos = pos_lab(position))
    p <- ggplot(avg, aes(value, pos)) +
      geom_vline(xintercept = 0, colour = "grey70") +
      geom_point(data = pts, colour = GREY, size = 1.4, alpha = 0.8, na.rm = TRUE) +
      geom_point(aes(colour = who, shape = who), size = 3.6, stroke = 1.2, na.rm = TRUE) +
      scale_colour_manual(values = marks, name = NULL) +
      scale_shape_manual(values = c(16, 18, 15), name = NULL) +
      facet_wrap(~ model, scales = "free_x") +
      labs(title = title, subtitle = sprintf("Grey dots = each top-%d team (by %s), %d season", TOP_B, rank_name, SEASON),
           x = xlab, y = NULL) + theme_g
    ggsave(file.path(OUT_DIR, file), p, width = 10, height = 5.5, dpi = 150, bg = "white")
  }
  chart_group("quality", sprintf("%s vs top programs: quality of minutes at each position", FOCUS_TEAM),
              "Minutes-weighted rating per 90 (GAR in goals, PAR in league points)", "01_position_quality.png")
  chart_group("contribution", sprintf("%s vs top programs: total value from each position", FOCUS_TEAM),
              "Rating per game from the position (GAR in goals, PAR in league points)", "02_position_contribution.png")

  gp <- summ %>% filter(measure == "quality") %>%
    select(position, model, gap_a_sd, gap_b_sd) %>%
    pivot_longer(c(gap_a_sd, gap_b_sd), names_to = "vs", values_to = "gap") %>%
    mutate(vs = factor(recode(vs, gap_a_sd = sprintf("vs top %d", TOP_A), gap_b_sd = sprintf("vs top %d", TOP_B)),
                       levels = sprintf("vs top %d", c(TOP_A, TOP_B))),
           pos = factor(POS_NAMES[as.character(position)],
                        levels = POS_NAMES[as.character(pri$position[order(-pri$gap_top_a)])]))
  p3 <- ggplot(gp, aes(gap, pos, fill = model)) +
    geom_vline(xintercept = 0, colour = "grey40") +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    scale_fill_manual(values = c(GAR = MAROON, PAR = SLATE), name = NULL) +
    facet_wrap(~ vs) +
    labs(title = sprintf("Where %s is furthest behind", FOCUS_TEAM),
         subtitle = "Gap in quality of minutes, in team-to-team standard deviations at the position. Biggest gaps at the top.",
         x = "Gap (negative = behind)", y = NULL) + theme_g
  ggsave(file.path(OUT_DIR, "03_gaps_ranked.png"), p3, width = 10, height = 5.5, dpi = 150, bg = "white")
}
cat(sprintf("\nOutputs in %s/\n", OUT_DIR))
