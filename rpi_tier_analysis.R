# =============================================================================
# Preliminary analysis of the six RPI tiers
# Input : rpi_standings_all.csv (from scrape_rpi.R)
# Output: tables printed to the console + PNG charts in ./tier_output/
# =============================================================================

# install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "scales", "ggrepel"))
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggrepel)

OUT_DIR <- "tier_output"
dir.create(OUT_DIR, showWarnings = FALSE)

df_raw <- read_csv("rpi_standings_all.csv", show_col_types = FALSE)

# The source spells some schools differently across seasons (e.g. "UNC
# Wilmington" in 2015, "UNCW" after). Map every variant to one name so each
# program's history stays together. Add to this list if you spot others.
name_fixes <- c(
  "Albany (N.Y.)"             = "UAlbany",
  "Cal Baptist"               = "California Baptist",
  "Central Connecticut State" = "Central Connecticut",
  "Connecticut"               = "UConn",
  "Detroit"                   = "Detroit Mercy",
  "Dixie State"               = "Utah Tech",
  "East Tennessee State"      = "ETSU",
  "Fairleigh Dickinson"       = "FDU",
  "Florida Gulf Coast"        = "FGCU",
  "Fort Wayne"                = "Purdue Fort Wayne",
  "IPFW"                      = "Purdue Fort Wayne",
  "HBU"                       = "HCU",
  "Houston Baptist"           = "HCU",
  "IUPUI"                     = "IU Indy",
  "Incarnate Word"            = "UIW",
  "UMKC"                      = "Kansas City",
  "LIU Brooklyn"              = "LIU",
  "Loyola Marymount"          = "LMU",
  "Massachusetts"             = "UMass",
  "Saint Francis (Pa.)"       = "Saint Francis",
  "UNC Greensboro"            = "UNCG",
  "UNC Wilmington"            = "UNCW",
  "RMU"                       = "Robert Morris"
)
df_raw <- df_raw %>%
  mutate(team = str_squish(team),
         team = str_replace(team, "San Jos.{1,8} State", "San Jose State"),  # accent/encoding variants
         team = recode(team, !!!name_fixes))

# ---- 1. Clean ---------------------------------------------------------------

# The scraped 2018 ACC rows are a copy of 2020-21 data, so they're always
# removed. If you've hand-collected the real 2018 ACC numbers, save them as
# acc_2018_manual.csv (columns: team, overall_w, overall_l, overall_t, rpi_rank)
# next to this script and they'll be added in their place.
MANUAL_FILE <- "acc_2018_manual.csv"

df_raw <- df_raw %>%
  filter(!(season_year == 2018 & conference == "Atlantic Coast Conference"))

if (file.exists(MANUAL_FILE)) {
  manual <- read_csv(MANUAL_FILE, show_col_types = FALSE) %>%
    mutate(team = str_squish(team)) %>%
    filter(!is.na(team), team != "")
  
  needed <- c("team", "overall_w", "overall_l", "overall_t", "rpi_rank")
  missing_cols <- setdiff(needed, names(manual))
  if (length(missing_cols)) stop(MANUAL_FILE, " is missing columns: ",
                                 paste(missing_cols, collapse = ", "))
  
  manual <- manual %>%
    select(all_of(needed)) %>%
    mutate(across(-team, as.numeric),
           season_year = 2018, season = "2018",
           conference = "Atlantic Coast Conference",
           post_url = "manual entry")
  
  # Warn about team names that don't match the spelling used in other seasons
  known <- unique(df_raw$team)
  unknown <- setdiff(manual$team, known)
  if (length(unknown)) warning("These names don't match any team in the main CSV ",
                               "(check spelling): ", paste(unknown, collapse = ", "))
  
  # Hand-collected ranks come from a different source than the blog, so a few
  # may collide with ranks other 2018 teams already have. List them for review.
  clash <- df_raw %>%
    filter(season_year == 2018) %>%
    select(rpi_rank, other_team = team) %>%
    inner_join(manual %>% select(rpi_rank, team), by = "rpi_rank")
  if (nrow(clash)) {
    message("Note: these 2018 RPI ranks are shared with another team (sources differ):")
    print(clash)
  }
  
  df_raw <- bind_rows(df_raw, manual)
  message("Added ", nrow(manual), " hand-collected 2018 ACC rows from ", MANUAL_FILE)
  
  # Optional: also save a combined CSV so other scripts can use it
  write_csv(df_raw, "rpi_standings_all_fixed.csv")
}

df <- df_raw %>%
  mutate(games    = overall_w + overall_l + overall_t,
         win_pct  = (overall_w + 0.5 * overall_t) / games,
         ppg      = (3 * overall_w + overall_t) / games,      # soccer points per game
         tie_rate = overall_t / games,
         conf_games   = conf_w + conf_l + conf_t,
         conf_win_pct = (conf_w + 0.5 * conf_t) / conf_games) %>%
  filter(
    season_year <= 2025,                                  # 2026 still in progress
    games > 0                                             # 2020-21: teams that didn't play
  )

# Seasons to use for most comparisons. The 2020-21 COVID season had short,
# regional schedules, so it's kept in the data but excluded from summaries.
MAIN_SEASONS <- setdiff(2015:2025, 2020)

# ---- 2. Tiers, computed per season ------------------------------------------
# The number of ranked teams changes by season (about 205 to 213), so the
# quartile cutoffs below rank 25 are computed from each season's max rank
# instead of a fixed 213.

tier_levels <- c("Champion (1-10)", "Elite (11-25)", "Q1", "Q2", "Q3", "Q4")

df <- df %>%
  group_by(season_year) %>%
  mutate(n_ranked = max(rpi_rank),
         inc      = (n_ranked - 25) / 4,
         tier = case_when(
           rpi_rank <= 10            ~ tier_levels[1],
           rpi_rank <= 25            ~ tier_levels[2],
           rpi_rank <= 25 + 1 * inc  ~ tier_levels[3],
           rpi_rank <= 25 + 2 * inc  ~ tier_levels[4],
           rpi_rank <= 25 + 3 * inc  ~ tier_levels[5],
           TRUE                      ~ tier_levels[6]),
         tier = factor(tier, levels = tier_levels)) %>%
  ungroup() %>%
  select(-inc)

main <- df %>% filter(season_year %in% MAIN_SEASONS)

# Chart colors: one blue ramp, darkest = best tier
tier_cols <- setNames(c("#0d366b", "#1c5cab", "#2a78d6", "#5598e7", "#6da7ec", "#86b6ef"),
                      tier_levels)
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title.position = "plot",
                  plot.title = element_text(face = "bold")))

# ---- 3. What does each tier look like? --------------------------------------

tier_profile <- main %>%
  group_by(tier) %>%
  summarise(team_seasons   = n(),
            win_pct        = mean(win_pct),
            ppg            = mean(ppg),
            tie_rate       = mean(tie_rate),
            losses_per_szn = mean(overall_l),
            conf_win_pct   = mean(conf_win_pct, na.rm = TRUE),
            median_sos     = median(sos_rank, na.rm = TRUE),
            .groups = "drop")
message("\n== Tier profile (", min(MAIN_SEASONS), "-", max(MAIN_SEASONS), ", excl. 2020-21) ==")
print(tier_profile, width = Inf)
write_csv(tier_profile, file.path(OUT_DIR, "tier_profile.csv"))

# How much of RPI is schedule? Win % vs SOS rank, colored by tier.
# Six distinct hues running cool (best) to warm (worst), checked for
# colorblind separation. Each tier also gets its own point shape, so tiers
# can be told apart without relying on color alone.
tier_cols_distinct <- setNames(
  c("#184f95", "#4c9be8", "#1baf7a", "#d6a000", "#ea5f2a", "#9b2d5a"),
  tier_levels)
tier_shapes <- setNames(c(16, 17, 15, 18, 25, 4), tier_levels)

p_sos <- ggplot(main, aes(sos_rank, win_pct, color = tier, fill = tier, shape = tier)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = tier_cols_distinct, name = NULL) +
  scale_fill_manual(values = tier_cols_distinct, name = NULL) +
  scale_shape_manual(values = tier_shapes, name = NULL) +
  guides(color = guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Win % and strength of schedule by RPI tier",
       subtitle = "Lower SOS rank = harder schedule. Top tiers need both a winning record and a tough schedule.",
       x = "SOS rank", y = "Win % (ties = half)")
ggsave(file.path(OUT_DIR, "winpct_vs_sos.png"), p_sos, width = 9, height = 6, dpi = 150, bg = "white")

# Same data as small multiples: one panel per tier, with every other team in
# light gray behind it. Easier to read than six colors in one cloud.
p_sos_facet <- ggplot(main, aes(sos_rank, win_pct)) +
  geom_point(data = select(main, -tier), color = "#d9d8d4", size = 1.2) +
  geom_point(aes(color = tier), size = 1.6, alpha = 0.85) +
  scale_color_manual(values = tier_cols_distinct, guide = "none") +
  facet_wrap(~ tier, nrow = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Win % and strength of schedule, one panel per RPI tier",
       subtitle = "Gray = all other team-seasons. Lower SOS rank = harder schedule.",
       x = "SOS rank", y = "Win % (ties = half)") +
  theme(strip.text = element_text(face = "bold", hjust = 0))
ggsave(file.path(OUT_DIR, "winpct_vs_sos_by_tier.png"), p_sos_facet, width = 10, height = 6.5, dpi = 150, bg = "white")

# ---- 4. Which conferences fill the top tiers? -------------------------------

conf_tiers <- main %>%
  count(conference, tier) %>%
  group_by(conference) %>%
  mutate(share_of_conf = n / sum(n)) %>%
  ungroup()

top25_by_conf <- main %>%
  filter(rpi_rank <= 25) %>%
  count(conference, name = "top25_slots") %>%
  mutate(share = top25_slots / sum(top25_slots)) %>%
  arrange(desc(top25_slots))
message("\n== Share of all top-25 RPI slots by conference ==")
print(top25_by_conf, n = Inf)
write_csv(conf_tiers, file.path(OUT_DIR, "conference_by_tier.csv"))

p_conf <- top25_by_conf %>%
  slice_head(n = 12) %>%
  mutate(conference = str_remove(conference, " Conference$"),
         conference = reorder(conference, top25_slots)) %>%
  ggplot(aes(top25_slots, conference)) +
  geom_col(fill = "#2a78d6", width = 0.7) +
  geom_text(aes(label = top25_slots), hjust = -0.2, size = 3.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Top-25 RPI finishes by conference",
       subtitle = paste0(min(MAIN_SEASONS), "-", max(MAIN_SEASONS), ", excluding 2020-21"),
       x = "Team-seasons ranked 1-25", y = NULL)
ggsave(file.path(OUT_DIR, "top25_by_conference.png"), p_conf, width = 8, height = 5.5, dpi = 150, bg = "white")

# ---- 5. How sticky are the tiers? Year-to-year movement ---------------------

transitions <- df %>%
  select(team, season_year, tier) %>%
  inner_join(df %>% select(team, season_year, next_tier = tier) %>%
               mutate(season_year = season_year - 1),
             by = c("team", "season_year")) %>%
  filter(season_year %in% MAIN_SEASONS, (season_year + 1) %in% MAIN_SEASONS)

trans_mat <- transitions %>%
  count(tier, next_tier) %>%
  complete(tier, next_tier, fill = list(n = 0)) %>%
  group_by(tier) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

message("\n== Tier next season (row = this season, % of row) ==")
print(trans_mat %>% select(-n) %>%
        pivot_wider(names_from = next_tier, values_from = pct, values_fill = 0) %>%
        mutate(across(-tier, ~ round(100 * .x))), width = Inf)
write_csv(trans_mat, file.path(OUT_DIR, "tier_transitions.csv"))

p_trans <- ggplot(trans_mat, aes(next_tier, tier, fill = pct)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1),
                color = pct > 0.4), size = 3.5) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#1f1f1f"), guide = "none") +
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b", labels = scales::percent, name = NULL) +
  scale_y_discrete(limits = rev) +
  scale_x_discrete(labels = function(x) str_replace(x, " \\(", "\n(")) +
  labs(title = "Where teams finish the following season",
       subtitle = "Each row sums to 100%",
       x = "Tier next season", y = "Tier this season") +
  theme(panel.grid = element_blank())
ggsave(file.path(OUT_DIR, "tier_transitions.png"), p_trans, width = 8, height = 5.5, dpi = 150, bg = "white")

# ---- 6. Perennial programs: your comparison group for the roster study ------

program_history <- main %>%
  group_by(team) %>%
  summarise(seasons          = n(),
            top10_finishes   = sum(rpi_rank <= 10),
            top25_finishes   = sum(rpi_rank <= 25),
            median_rpi       = median(rpi_rank),
            best_rpi         = min(rpi_rank),
            latest_conf      = conference[which.max(season_year)],
            .groups = "drop") %>%
  arrange(desc(top10_finishes), desc(top25_finishes), median_rpi)

message("\n== Programs with the most top-10 / top-25 RPI finishes ==")
print(head(program_history, 25), n = 25, width = Inf)
write_csv(program_history, file.path(OUT_DIR, "program_history.csv"))

# ---- 6b. Program tiers: classify each program by its 10-year record ---------
# Each season's rank is turned into a percentile of that season's field
# (rank 1 of 206 -> 0.5%), because the number of ranked teams changes by year.
# Programs are ordered by their average percentile, then split the same way
# as the season tiers: top 10 programs = Champion, 11-25 = Elite, and the rest
# into four equal groups. Programs with fewer than MIN_SEASONS seasons in the
# data (e.g. Atlantic 10 teams, which are only available from 2024) are listed
# separately instead of being forced into a tier on 1-2 seasons.

MIN_SEASONS <- 6

program_scores <- main %>%
  mutate(pctile = rpi_rank / n_ranked) %>%
  group_by(team) %>%
  summarise(seasons        = n(),
            avg_pctile     = mean(pctile),
            avg_rpi        = mean(rpi_rank),
            median_rpi     = median(rpi_rank),
            best_rpi       = min(rpi_rank),
            worst_rpi      = max(rpi_rank),
            top10_finishes = sum(rpi_rank <= 10),
            top25_finishes = sum(rpi_rank <= 25),
            avg_win_pct    = mean(win_pct),
            latest_conf    = str_remove(conference[which.max(season_year)], " Conference$"),
            .groups = "drop")

ranked_programs <- program_scores %>%
  filter(seasons >= MIN_SEASONS) %>%
  arrange(avg_pctile, median_rpi) %>%
  mutate(program_rank = row_number())

n_prog <- nrow(ranked_programs)
p_inc  <- (n_prog - 25) / 4

program_tiers <- ranked_programs %>%
  mutate(program_tier = case_when(
    program_rank <= 10              ~ tier_levels[1],
    program_rank <= 25              ~ tier_levels[2],
    program_rank <= 25 + 1 * p_inc  ~ tier_levels[3],
    program_rank <= 25 + 2 * p_inc  ~ tier_levels[4],
    program_rank <= 25 + 3 * p_inc  ~ tier_levels[5],
    TRUE                            ~ tier_levels[6]),
    program_tier = factor(program_tier, levels = tier_levels)) %>%
  relocate(program_rank, program_tier, team, latest_conf)

too_few <- program_scores %>%
  filter(seasons < MIN_SEASONS) %>%
  arrange(avg_pctile)

message("\n== Program tiers (", min(MAIN_SEASONS), "-", max(MAIN_SEASONS),
        ", excl. 2020-21; ", n_prog, " programs with ", MIN_SEASONS, "+ seasons) ==")
for (t in tier_levels) {
  tt <- program_tiers %>% filter(program_tier == t)
  message("\n", t, " (", nrow(tt), " programs):")
  message(paste(strwrap(paste(tt$team, collapse = ", "), width = 90), collapse = "\n"))
}
message("\nNot tiered (fewer than ", MIN_SEASONS, " seasons of data): ",
        paste(too_few$team, collapse = ", "))

message("\n== Program tier summary ==")
print(program_tiers %>%
        group_by(program_tier) %>%
        summarise(programs = n(),
                  avg_rpi = mean(avg_rpi),
                  avg_win_pct = mean(avg_win_pct),
                  top10_finishes = sum(top10_finishes),
                  top25_finishes = sum(top25_finishes)),
      width = Inf)

message("\n== Virginia Tech's program ranking ==")
print(program_tiers %>% filter(team == "Virginia Tech") %>%
        select(program_rank, program_tier, seasons, avg_rpi, median_rpi, top25_finishes),
      width = Inf)

write_csv(program_tiers, file.path(OUT_DIR, "program_tiers.csv"))
write_csv(too_few,       file.path(OUT_DIR, "programs_not_tiered.csv"))

# ---- 7. Resume by tier: Quad records (only available 2024-2025) -------------

quad <- df %>%
  filter(season_year %in% c(2024, 2025), !is.na(q1_w)) %>%
  group_by(tier) %>%
  summarise(team_seasons  = n(),
            q1_games      = mean(q1_w + q1_l + q1_t),
            q1_wins       = mean(q1_w),
            q1_win_pct    = sum(q1_w + 0.5 * q1_t) / sum(q1_w + q1_l + q1_t),
            q3q4_losses   = mean(q3_l + q4_l),
            top100_wins   = mean(vs_top100_w),
            .groups = "drop")
message("\n== Quad resume by tier (2024-2025) ==")
print(quad, width = Inf)
write_csv(quad, file.path(OUT_DIR, "quad_resume_by_tier.csv"))

# ---- 8. Virginia Tech against the tiers -------------------------------------

vt <- df %>% filter(team == "Virginia Tech") %>% arrange(season_year)
message("\n== Virginia Tech by season ==")
print(vt %>% select(season_year, rpi_rank, tier, sos_rank, overall_w, overall_l, overall_t,
                    win_pct, conf_win_pct), n = Inf)

cutoffs <- df %>% distinct(season_year, n_ranked) %>%
  mutate(q1_cut = 25 + (n_ranked - 25) / 4,
         q2_cut = 25 + 2 * (n_ranked - 25) / 4)

p_vt <- ggplot(vt, aes(season_year, rpi_rank)) +
  geom_hline(yintercept = c(10, 25), linetype = "dashed", color = "#8a8a85") +
  annotate("text", x = min(vt$season_year), y = c(10, 25), label = c("Top 10", "Top 25"),
           vjust = -0.5, hjust = 0, size = 3.2, color = "#5c5c58") +
  geom_line(color = "#2a78d6", linewidth = 0.8) +
  geom_point(color = "#2a78d6", size = 2.8) +
  geom_text(aes(label = rpi_rank), vjust = -1, size = 3.2) +
  scale_y_reverse(limits = c(max(vt$rpi_rank) + 10, 0)) +
  scale_x_continuous(breaks = vt$season_year) +
  labs(title = "Virginia Tech final RPI rank by season",
       subtitle = "2020 = spring 2021 COVID season",
       x = NULL, y = "RPI rank (1 = best)")
ggsave(file.path(OUT_DIR, "virginia_tech_rpi.png"), p_vt, width = 9, height = 5, dpi = 150, bg = "white")

# VT's average season vs the average season in each tier
vt_vs_tiers <- bind_rows(
  tier_profile %>% transmute(group = as.character(tier), win_pct, ppg, median_sos),
  vt %>% filter(season_year %in% MAIN_SEASONS) %>%
    summarise(group = "Virginia Tech (avg season)",
              win_pct = mean(win_pct), ppg = mean(ppg), median_sos = median(sos_rank, na.rm = TRUE))
)
message("\n== Virginia Tech vs tier averages ==")
print(vt_vs_tiers, width = Inf)

message("\nCharts and CSVs saved to ./", OUT_DIR)

# =============================================================================
# 9. Virginia Tech & ACC charts  ->  ./vt_acc_output/
# =============================================================================

VT_DIR <- "vt_acc_output"
dir.create(VT_DIR, showWarnings = FALSE)

VT_COL   <- "#861F41"   # Virginia Tech maroon (highlight)
VT_ORNG  <- "#d0611a"   # Virginia Tech orange (second highlight)
CTX_COL  <- "#cfcec9"   # gray for context marks
INK_2    <- "#52514e"   # secondary text

save_vt <- function(plot, name, w = 9, h = 6) {
  ggsave(file.path(VT_DIR, name), plot, width = w, height = h, dpi = 150, bg = "white")
}
season_label <- function(y) ifelse(y == 2020, "20-21", as.character(y))

# Current ACC members (by 2025 conference), with their full 2015-2025 history.
# Cal, Stanford and SMU joined in 2024, so their earlier seasons were played
# in other conferences.
acc_teams <- df %>%
  filter(season_year == 2025, conference == "Atlantic Coast Conference") %>%
  pull(team) %>% unique()
acc <- df %>% filter(team %in% acc_teams)

# ---- 9.1 ACC heatmap: every program's RPI rank, every season ----------------
acc_order <- acc %>%
  filter(season_year %in% MAIN_SEASONS) %>%
  group_by(team) %>% summarise(avg = mean(rpi_rank / n_ranked)) %>%
  arrange(avg) %>% pull(team)

p_heat <- acc %>%
  mutate(team = factor(team, levels = rev(acc_order)),
         season = factor(season_label(season_year), levels = season_label(2015:2025)),
         face = ifelse(team == "Virginia Tech", "bold", "plain")) %>%
  ggplot(aes(season, team, fill = rpi_rank)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = rpi_rank, color = rpi_rank < 40), size = 3.1) +
  scale_fill_gradient(low = "#0d366b", high = "#e6eef9", trans = "log10",
                      breaks = c(1, 5, 10, 25, 50, 100, 200), name = "RPI rank") +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "#1f1f1f"), guide = "none") +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Every ACC program's final RPI rank, 2015-2025",
       x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(
          face  = ifelse(rev(acc_order) == "Virginia Tech", "bold", "plain"),
          color = ifelse(rev(acc_order) == "Virginia Tech", VT_COL, "#1f1f1f")))
save_vt(p_heat, "01_acc_rpi_heatmap.png", w = 10, h = 6.5)

# ---- 9.2 VT's RPI path against the rest of the ACC --------------------------
acc_median <- acc %>% group_by(season_year) %>% summarise(med = median(rpi_rank))

p_path <- ggplot(acc, aes(season_year, rpi_rank)) +
  geom_line(aes(group = team), color = CTX_COL, linewidth = 0.5) +
  geom_line(data = acc_median, aes(y = med), color = INK_2, linewidth = 0.9, linetype = "dashed") +
  geom_line(data = vt, color = VT_COL, linewidth = 1.2) +
  geom_point(data = vt, color = VT_COL, size = 2.8) +
  geom_label(data = vt, aes(label = rpi_rank), color = VT_COL, size = 3.2, fontface = "bold",
             vjust = -0.45, label.size = 0, label.padding = unit(0.12, "lines"), fill = "white") +
  annotate("text", x = 2025.15, y = tail(acc_median$med, 1), label = "ACC median",
           hjust = 0, size = 3.2, color = INK_2) +
  annotate("text", x = 2025.15, y = tail(vt$rpi_rank, 1), label = "Virginia Tech",
           hjust = 0, size = 3.4, color = VT_COL, fontface = "bold") +
  geom_hline(yintercept = 25, linetype = "dotted", color = "#8a8a85") +
  annotate("text", x = 2015, y = 25, label = "Top 25", vjust = -0.5, hjust = 0, size = 3, color = INK_2) +
  scale_y_reverse() +
  scale_x_continuous(breaks = 2015:2025, labels = season_label(2015:2025),
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(title = "Virginia Tech's RPI path vs. every current ACC program",
       subtitle = "Gray lines = other ACC programs", x = NULL, y = "RPI rank (1 = best)")
save_vt(p_path, "02_vt_vs_acc_rpi_path.png", w = 10, h = 6)

# ---- 9.3 Schedule vs results: VT's seasons against the ACC ------------------
tier_centers <- main %>% group_by(tier) %>%
  summarise(sos = median(sos_rank, na.rm = TRUE), wp = median(win_pct))

p_sched <- ggplot() +
  geom_point(data = filter(acc, team != "Virginia Tech"),
             aes(sos_rank, win_pct), color = CTX_COL, size = 2) +
  geom_point(data = tier_centers, aes(sos, wp, fill = tier), shape = 23, size = 4.5,
             color = "white", stroke = 0.8) +
  geom_point(data = vt, aes(sos_rank, win_pct), color = VT_COL, size = 3.2) +
  geom_text_repel(data = vt, aes(sos_rank, win_pct, label = season_label(season_year)),
                  color = VT_COL, size = 3.3, fontface = "bold", seed = 1,
                  min.segment.length = 0.3, box.padding = 0.4) +
  scale_fill_manual(values = tier_cols_distinct, name = "Typical season\nin each tier") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Virginia Tech plays an elite schedule but posts a mid-table record",
       subtitle = "Maroon = VT seasons. Gray = other ACC team-seasons. Diamonds = median SOS and win % of each RPI tier.",
       x = "SOS rank (lower = harder)", y = "Win % (ties = half)")
save_vt(p_sched, "03_vt_schedule_vs_results.png", w = 10, h = 6.5)

# ---- 9.4 What win % does VT's schedule require? -----------------------------
# Teams that played a schedule like VT's (SOS rank within 20 of VT's median),
# and the win % they posted, by the tier they finished in.
vt_sos <- median(vt$sos_rank[vt$season_year %in% MAIN_SEASONS], na.rm = TRUE)
similar <- main %>% filter(abs(sos_rank - vt_sos) <= 20)
needed <- similar %>% group_by(tier) %>%
  summarise(n = n(), med = median(win_pct), lo = quantile(win_pct, 0.25),
            hi = quantile(win_pct, 0.75), .groups = "drop")

p_need <- ggplot(needed, aes(y = tier)) +
  geom_linerange(aes(xmin = lo, xmax = hi, color = tier), linewidth = 6, alpha = 0.35) +
  geom_point(aes(x = med, color = tier), size = 4) +
  geom_text(aes(x = med, label = scales::percent(med, accuracy = 1)), vjust = -1.3, size = 3.3) +
  geom_point(data = filter(vt, season_year %in% MAIN_SEASONS),
             aes(x = win_pct, y = tier), shape = 21, fill = "white", color = VT_COL,
             size = 3, stroke = 1.3) +
  scale_color_manual(values = tier_cols_distinct, guide = "none") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = paste0("What a schedule like Virginia Tech's takes: win % by finishing tier"),
       subtitle = paste0("Teams with SOS rank ", vt_sos - 20, "-", vt_sos + 20,
                         " (VT's median SOS is ", vt_sos, "). Dot = median, bar = middle 50%. ",
                         "Open maroon circles = VT seasons, on the tier VT finished in."),
       x = "Win % (ties = half)", y = NULL)
save_vt(p_need, "04_win_pct_needed_with_vt_schedule.png", w = 10, h = 5.5)

# ---- 9.5 Conference vs non-conference results by season ---------------------
vt_split <- vt %>%
  filter(!is.na(conf_w)) %>%
  mutate(nc_w = overall_w - conf_w, nc_l = overall_l - conf_l, nc_t = overall_t - conf_t,
         `ACC games` = (conf_w + 0.5 * conf_t) / (conf_w + conf_l + conf_t),
         `Non-conference` = (nc_w + 0.5 * nc_t) / (nc_w + nc_l + nc_t)) %>%
  select(season_year, `ACC games`, `Non-conference`) %>%
  pivot_longer(-season_year, names_to = "games", values_to = "win_pct") %>%
  mutate(season = factor(season_label(season_year), levels = season_label(2015:2025)))

p_split <- ggplot(vt_split, aes(win_pct, season)) +
  geom_line(aes(group = season), color = CTX_COL, linewidth = 2) +
  geom_point(aes(color = games), size = 3.8) +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "#8a8a85") +
  scale_color_manual(values = c(`ACC games` = VT_COL, `Non-conference` = VT_ORNG), name = NULL) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_discrete(limits = rev) +
  labs(title = "Virginia Tech: ACC games vs. non-conference games",
       x = "Win %", y = NULL) +
  theme(legend.position = "top", legend.justification = "left")
save_vt(p_split, "05_vt_conf_vs_nonconf.png", w = 9, h = 6)

# ---- 9.6 ACC programs' 10-year range and program tier -----------------------
acc_prog <- program_tiers %>% filter(team %in% acc_teams) %>%
  mutate(team = reorder(team, -avg_rpi))

p_range <- ggplot(acc_prog, aes(y = team)) +
  geom_linerange(aes(xmin = best_rpi, xmax = worst_rpi), color = CTX_COL, linewidth = 3) +
  geom_point(aes(x = median_rpi, color = program_tier), size = 4) +
  geom_text(aes(x = worst_rpi, label = paste0(top25_finishes, " top-25")),
            hjust = -0.15, size = 3, color = INK_2) +
  scale_color_manual(values = tier_cols_distinct, name = "10-year\nprogram tier", drop = TRUE) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(title = "ACC programs: typical finish and range over 10 seasons",
       subtitle = "Dot = median RPI rank; bar = best to worst finish (2015-2025, excl. 2020-21)",
       x = "RPI rank (1 = best)", y = NULL) +
  theme(axis.text.y = element_text(
    face  = ifelse(levels(acc_prog$team) == "Virginia Tech", "bold", "plain"),
    color = ifelse(levels(acc_prog$team) == "Virginia Tech", VT_COL, "#1f1f1f")))
save_vt(p_range, "06_acc_program_ranges.png", w = 9.5, h = 6.5)

# ---- 9.7 How strong is the ACC each season? ---------------------------------
acc_strength <- df %>%
  filter(conference == "Atlantic Coast Conference", season_year %in% MAIN_SEASONS) %>%
  group_by(season_year) %>%
  summarise(`Top 10` = sum(rpi_rank <= 10), `11-25` = sum(rpi_rank > 10 & rpi_rank <= 25),
            teams = n(), .groups = "drop") %>%
  pivot_longer(c(`Top 10`, `11-25`), names_to = "band", values_to = "n") %>%
  mutate(band = factor(band, levels = c("11-25", "Top 10")))

p_str <- ggplot(acc_strength, aes(factor(season_year), n, fill = band)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.6) +
  geom_text(data = acc_strength %>% group_by(season_year, teams) %>% summarise(n = sum(n), .groups = "drop"),
            aes(factor(season_year), n, label = paste0(n, " of ", teams)), inherit.aes = FALSE,
            vjust = -0.5, size = 3.2, color = INK_2) +
  scale_fill_manual(values = c(`Top 10` = tier_cols_distinct[[1]], `11-25` = tier_cols_distinct[[2]]),
                    name = "RPI finish", breaks = c("Top 10", "11-25")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "How many ACC teams finish in the RPI top 25 each season",
       subtitle = "Label = ACC teams in the top 25, out of ACC teams in the data (2018 depends on the manual file)",
       x = NULL, y = "ACC teams") +
  theme(panel.grid.major.x = element_blank(), legend.position = "top", legend.justification = "left")
save_vt(p_str, "07_acc_top25_count_by_season.png", w = 9, h = 5.5)

# ---- 9.8 Resume (2024-2025): VT vs top-tier averages ------------------------
resume_metrics <- function(d) d %>%
  transmute(`Q1 games` = q1_w + q1_l + q1_t, `Q1 wins` = q1_w,
            `Wins vs top-100` = vs_top100_w, `Q3/Q4 losses` = q3_l + q4_l)

resume <- bind_rows(
  df %>% filter(season_year %in% c(2024, 2025), !is.na(q1_w),
                tier %in% tier_levels[1:3]) %>%
    group_by(group = as.character(tier)) %>%
    group_modify(~ summarise(resume_metrics(.x), across(everything(), mean))) %>% ungroup(),
  vt %>% filter(season_year %in% c(2024, 2025)) %>%
    mutate(group = paste("Virginia Tech", season_year)) %>%
    { bind_cols(select(., group), resume_metrics(.)) }
) %>%
  pivot_longer(-group, names_to = "metric", values_to = "value") %>%
  mutate(group = factor(group, levels = c(tier_levels[1:3], "Virginia Tech 2024", "Virginia Tech 2025")),
         metric = factor(metric, levels = c("Q1 games", "Q1 wins", "Wins vs top-100", "Q3/Q4 losses")))

resume_cols <- c(tier_cols_distinct[1:3], `Virginia Tech 2024` = VT_ORNG, `Virginia Tech 2025` = VT_COL)

p_resume <- ggplot(resume, aes(value, group, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(value, 1)), hjust = -0.2, size = 3.2) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = resume_cols, guide = "none") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)), breaks = scales::breaks_pretty(n = 3)) +
  labs(title = "Virginia Tech's resume vs. the average Champion, Elite and Q1 team",
       subtitle = "2024-2025 seasons (Quad records are only available from 2024). Q3/Q4 losses: lower is better.",
       x = NULL, y = NULL) +
  theme(strip.text = element_text(face = "bold", hjust = 0), panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.6, "lines"))
save_vt(p_resume, "08_vt_resume_vs_tiers.png", w = 11, h = 4.8)

message("Virginia Tech / ACC charts saved to ./", VT_DIR)