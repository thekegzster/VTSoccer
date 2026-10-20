# =============================================================================
# Preliminary analysis of the six RPI tiers
# Input : rpi_standings_all.csv (from scrape_rpi.R)
# Output: tables printed to the console + PNG charts in ./tier_output/
# =============================================================================

# install.packages(c("readr", "dplyr", "tidyr", "ggplot2", "stringr"))
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

OUT_DIR <- "tier_output"
dir.create(OUT_DIR, showWarnings = FALSE)

df_raw <- read_csv("rpi_standings_all.csv", show_col_types = FALSE)

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