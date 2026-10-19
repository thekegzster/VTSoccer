# =============================================================================
# Scrape final Massey Ratings, NCAA D1 men's soccer, past seasons
# Source: masseyratings.com/csoc<YEAR>/ncaa-d1/ratings
#
# The ratings table is filled in by JavaScript after the page loads, and the
# underlying JSON request has encoded values. So this script opens each season
# in a hidden Chrome window (via chromote), waits for the table to load, and
# reads the table exactly as it is displayed.
#
# Requires Google Chrome installed on your computer.
# Output: massey_ratings_all.csv (one row per team per season)
# =============================================================================

# install.packages(c("chromote", "rvest", "dplyr", "purrr", "stringr", "readr", "tibble", "tidyr"))
library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(tidyr)

# ---- Settings ---------------------------------------------------------------

SEASONS   <- 2025          # test with a single year first, e.g. 2025
CACHE_DIR <- "C:/massey_cache"  # outside OneDrive; rendered pages saved here
OUT_FILE  <- "massey_ratings_all.csv"
DELAY_SEC <- 3                  # pause between seasons (be polite)
TIMEOUT   <- 45                 # max seconds to wait for a table to load

season_url <- function(yr) paste0("https://masseyratings.com/csoc", yr, "/ncaa-d1/ratings")

# ---- Render a page and return its final HTML --------------------------------

b <- ChromoteSession$new()

js <- function(expr) b$Runtime$evaluate(expr)$result$value

render_page <- function(url) {
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(url, wait_ = FALSE)
  b$wait_for(loaded)
  
  # wait until the "Please wait" placeholder is gone and the table has rows
  start <- Sys.time()
  repeat {
    waiting <- isTRUE(js("document.body.innerText.includes('Please wait while the data loads')"))
    n_rows  <- js("document.querySelectorAll('table tr').length")
    if (!waiting && !is.null(n_rows) && n_rows > 50) break
    if (difftime(Sys.time(), start, units = "secs") > TIMEOUT) {
      warning("Timed out waiting for table: ", url)
      break
    }
    Sys.sleep(1)
  }
  Sys.sleep(1)  # let the last rows finish drawing
  js("document.documentElement.outerHTML")
}

get_season_html <- function(yr) {
  dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(CACHE_DIR, paste0("csoc", yr, ".html"))
  if (!file.exists(f)) {
    message("Rendering ", yr, " ...")
    html <- render_page(season_url(yr))
    writeLines(html, f, useBytes = TRUE)
    Sys.sleep(DELAY_SEC)
  } else {
    message("Using cached ", yr)
  }
  read_html(f, encoding = "UTF-8")
}

# ---- Parse the displayed ratings table --------------------------------------
# Each cell can hold two stacked values (e.g. rank on top, rating below; or
# team on top, conference below). We split each cell on line breaks:
#   first value  -> <title>         (e.g. rat = rank)
#   second value -> <title>_detail  (e.g. rat_detail = rating value)

parse_massey <- function(doc, yr) {
  tables <- html_elements(doc, "table")
  if (length(tables) == 0) { warning("No table for ", yr); return(NULL) }
  
  # pick the table with the most rows
  tbl <- tables[[which.max(map_int(tables, ~ length(html_elements(.x, "tr"))))]]
  
  header_cells <- html_elements(tbl, "tr th")
  titles <- str_squish(html_text2(header_cells))
  titles <- titles[titles != ""]
  first_title_idx <- which(titles == "Team")[1]
  titles <- unique(titles[first_title_idx:length(titles)])  # header repeats every 20 rows
  
  rows <- html_elements(tbl, "tr")
  map_dfr(rows, function(tr) {
    tds <- html_elements(tr, "td")
    if (length(tds) < 3) return(NULL)             # header / spacer rows
    parts <- map(tds, ~ {
      p <- str_split(html_text2(.x), "\n")[[1]]
      p <- str_squish(p)
      p[p != ""]
    })
    if (length(parts[[1]]) == 0 || parts[[1]][1] == "Team") return(NULL)
    
    n <- min(length(parts), length(titles))
    vals <- list()
    for (i in seq_len(n)) {
      key <- str_to_lower(str_replace_all(titles[i], "[^A-Za-z0-9]+", "_"))
      vals[[key]] <- parts[[i]][1]
      if (length(parts[[i]]) > 1) vals[[paste0(key, "_detail")]] <- parts[[i]][2]
    }
    as_tibble(vals)
  }) %>%
    mutate(season_year = yr, .before = 1)
}

# ---- Run --------------------------------------------------------------------

massey_raw <- map_dfr(SEASONS, function(yr) {
  doc <- tryCatch(get_season_html(yr), error = function(e) {
    warning(yr, ": ", conditionMessage(e)); NULL
  })
  if (is.null(doc)) return(NULL)
  out <- parse_massey(doc, yr)
  message("  ", yr, ": ", nrow(out), " teams")
  out
})

b$close()

# Tidy names: team/conference, record/win pct, then rank + value per metric
massey <- massey_raw %>%
  rename(any_of(c(
    conference = "team_detail",
    record     = "rec",
    win_pct    = "rec_detail",
    rating_rank = "rat", rating = "rat_detail",
    power_rank  = "pwr", power  = "pwr_detail",
    off_rank    = "off", off    = "off_detail",
    def_rank    = "def", def    = "def_detail",
    sos_rank    = "sos", sos    = "sos_detail"
  ))) %>%
  mutate(across(any_of(c("win_pct", "rating_rank", "rating", "power_rank", "power",
                         "off_rank", "off", "def_rank", "def", "hfa",
                         "sos_rank", "sos")),
                ~ suppressWarnings(parse_number(.x)))) %>%
  separate_wider_delim(record, "-", names = c("w", "l", "t"),
                       too_few = "align_start", cols_remove = FALSE) %>%
  mutate(across(c(w, l, t), as.integer))

write_csv(massey, OUT_FILE)
message("Saved ", nrow(massey), " team-seasons to ", OUT_FILE)

# ---- Checks -----------------------------------------------------------------

# Expect roughly 200-210 teams per season and exactly one rating_rank == 1
massey %>% count(season_year, name = "teams") %>% print(n = Inf)
massey %>% filter(rating_rank == 1) %>% select(season_year, team, record, rating) %>% print(n = Inf)

# Look at the first rows of one season to confirm columns lined up correctly
massey %>% filter(season_year == max(season_year)) %>% head(10) %>% print(width = Inf)

# Virginia Tech by season
massey %>% filter(team == "Virginia Tech") %>%
  select(season_year, rating_rank, rating, record, sos_rank) %>% print(n = Inf)