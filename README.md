# Virginia Tech Men's Soccer: RPI & Program Analysis

Tools for collecting NCAA Division I men's soccer RPI data (2015–present) and comparing Virginia Tech against the ACC and the country's best programs. The long-term goal is to compare VT's roster makeup with elite programs once player data is added.

## Files

| File | What it does |
|---|---|
| `scrape_rpi.R` | Scrapes final RPI standings for every conference and season from the [RPI Update: Men's Soccer](https://rpiupdatemenssoccer.blogspot.com/) blog. Writes `rpi_standings_all.csv`. |
| `rpi_tier_analysis.R` | Splits teams into six RPI tiers, profiles each tier, ranks programs over 10 years, and makes the Virginia Tech / ACC charts. |
| `acc_2018_manual.csv` | Hand-entered 2018 ACC results (see Data notes). Optional. |
| `scrape_massey.R` | Work in progress: scrapes past Massey Ratings using a headless Chrome browser. |

## How to run

1. Install packages:
   ```r
   install.packages(c("rvest", "xml2", "dplyr", "purrr", "stringr", "tibble",
                      "readr", "tidyr", "ggplot2", "scales", "ggrepel"))
   ```
2. Run `scrape_rpi.R`. The full scrape takes about 15 minutes. Downloaded pages are cached in `rpi_cache/`, so later runs are fast.
3. (Optional) Fill in `acc_2018_manual.csv`.
4. Run `rpi_tier_analysis.R`.

## Tiers

Each season, teams are grouped by final RPI rank:

| Tier | RPI rank |
|---|---|
| Champion | 1–10 |
| Elite | 11–25 |
| Q1–Q4 | Remaining teams split into four equal groups |

The quartile cutoffs are calculated separately for each season, because the number of ranked teams varies (about 188–213). Programs are also given a **10-year tier** using their average RPI percentile across seasons.

## Outputs

**`tier_output/`**: tier profiles, year-to-year tier movement, conference share of top-25 finishes, Quad 1 resumes by tier, program history, and 10-year program tiers (`program_tiers.csv`).

**`vt_acc_output/`**: eight charts comparing Virginia Tech with the ACC, including an ACC RPI heatmap, VT's schedule strength vs. results, the win % needed to reach each tier, and VT's Quad 1 resume.

## Data notes

- **2026 is excluded** from the analysis because the season is still in progress.
- **2020-21 (COVID season)** is kept in the data but left out of summaries. Teams that didn't play that season are removed.
- **2018 ACC data from the source is wrong.** It's a copy of the 2020-21 data, so it's always dropped. Add the real numbers to `acc_2018_manual.csv` to fill the gap.
- **The Atlantic 10 is missing before 2024** because of broken links in the source archive. A10 programs (including Saint Louis) aren't given a 10-year tier.
- **Team names are standardized** (e.g., "UNC Wilmington" → "UNCW"). The mapping is at the top of `rpi_tier_analysis.R`; add to it if you find more variants.
- **This RPI is the blog's own calculation.** It closely matches the official NCAA RPI in most seasons but not all (it differs most in 2020-21).
- If the project is in a OneDrive folder on Windows, keep cache folders outside OneDrive (e.g., `C:/rpi_cache`) to avoid file-locking and long-path errors.

## Next steps

- Add player data: minutes and box-score stats from stats.ncaa.org, and class year, position and transfer history from school roster pages.
- Compare roster makeup (class year, transfers, returning minutes) between VT and the Champion/Elite programs, weighting by minutes played.
- Finish the Massey Ratings scraper.
