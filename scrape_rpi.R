# =============================================================================
# Scrape NCAA D1 men's soccer conference standings + RPI
# Source: rpiupdatemenssoccer.blogspot.com (Daniel Curry)
#
# How the site works:
#   1. The "Conference Standings Archive" is a published Google Sheet whose
#      cells link to one blog post per conference per season.
#   2. Each blog post embeds ANOTHER published Google Sheet (the standings
#      table). The numbers live in that sheet, not in the blog HTML.
# So: archive sheet -> post URLs -> embedded sheet URL -> parse table.
#
# Output: rpi_standings_all.csv (one row per team per season)
# =============================================================================

# install.packages(c("rvest", "xml2", "dplyr", "purrr", "stringr", "tibble", "readr"))
library(rvest)
library(xml2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)

# ---- Settings ---------------------------------------------------------------

# Published archive sheet (the year x conference link grid)
INDEX_URL <- paste0(
  "https://docs.google.com/spreadsheets/d/e/",
  "2PACX-1vRuPgCxpscL-HYhxPU7pc5nyn5UWUaM-fKXIsIqcuBB4HVI3U-s3wLRGJ9j9ZzRXlj1jo_jkYdYUuPA/",
  "pubhtml?single=true&gid=1042019865&range=b1:n29&chrome=false&widget=false"
)

# Set to NULL to scrape everything, or narrow it down while testing, e.g.
#   SEASONS     <- c("2024", "2025")
#   CONFERENCES <- c("Atlantic Coast Conference")
SEASONS     <- NULL
CONFERENCES <- NULL

DELAY_SEC <- 1.5          # pause between live requests (be polite)
CACHE_DIR <- "rpi_cache"  # raw HTML is saved here so re-runs don't re-download
OUT_FILE  <- "rpi_standings_all.csv"

# ---- Helpers ----------------------------------------------------------------

# Short, unique cache filename for a URL (32-character MD5 hash).
# Full URLs as filenames exceed Windows' 260-character path limit.
url_hash <- function(url) {
  tf <- tempfile()
  writeLines(url, tf, useBytes = TRUE)
  h <- unname(tools::md5sum(tf))
  unlink(tf)
  h
}

# Download a page once, cache it to disk, retry on failure
fetch_html <- function(url, tries = 3) {
  dir.create(CACHE_DIR, showWarnings = FALSE)
  fname <- file.path(CACHE_DIR, paste0(url_hash(url), ".html"))
  
  if (!file.exists(fname)) {
    for (i in seq_len(tries)) {
      ok <- tryCatch({
        download_html(url, file = fname, quiet = TRUE)
        TRUE
      }, error = function(e) {
        message("  attempt ", i, " failed for ", url, ": ", conditionMessage(e))
        FALSE
      })
      Sys.sleep(DELAY_SEC)
      if (ok) break
    }
    if (!file.exists(fname)) return(NULL)
  }
  read_html(fname, encoding = "UTF-8")
}

# Google wraps sheet links as https://www.google.com/url?q=<real url>&sa=...
unwrap_google_link <- function(href) {
  inner <- str_match(href, "[?&]q=([^&]+)")[, 2]
  out <- ifelse(is.na(inner), href, URLdecode(inner))
  str_replace(out, "^http://", "https://")
}

# Parse a published-Google-Sheet table into text + link matrices.
# Skips row-number headers and freeze-bar cells; expands merged (colspan) cells.
parse_waffle <- function(doc) {
  tbl <- html_element(doc, "table.waffle")
  if (inherits(tbl, "xml_missing")) tbl <- html_element(doc, "table")
  if (inherits(tbl, "xml_missing")) return(NULL)
  
  rows <- html_elements(tbl, "tbody tr")
  parsed <- map(rows, function(tr) {
    tds <- html_elements(tr, "td")
    cls <- coalesce(html_attr(tds, "class"), "")
    tds <- tds[!str_detect(cls, "freezebar")]
    txt <- character(0); href <- character(0)
    for (td in tds) {
      span <- suppressWarnings(as.integer(html_attr(td, "colspan")))
      if (is.na(span)) span <- 1L
      t <- str_squish(html_text2(td))
      h <- html_attr(html_element(td, "a"), "href")
      txt  <- c(txt,  rep(t, span))
      href <- c(href, rep(h, span))
    }
    list(txt = txt, href = href)
  })
  
  w <- max(map_int(parsed, ~ length(.x$txt)), 0L)
  pad <- function(v, fill) c(v, rep(fill, w - length(v)))
  list(
    text = do.call(rbind, map(parsed, ~ pad(.x$txt, ""))),
    href = do.call(rbind, map(parsed, ~ pad(.x$href, NA_character_)))
  )
}

slugify <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]+", "-")

# Clean column-group labels from the sheet header into tidy names
clean_label <- function(x) {
  x <- str_to_lower(str_squish(x))
  case_when(
    x == "conf"    ~ "conf",
    x == "pts"     ~ "conf_pts",
    x == "overall" ~ "overall",
    x == "rpi"     ~ "rpi_rank",
    x == "sos"     ~ "sos_rank",
    x == "last 8"  ~ "last8",
    str_detect(x, "^quad") ~ str_replace(x, "quad\\s*", "q"),
    str_detect(x, "^<=")   ~ paste0("vs_top", str_extract(x, "\\d+")),
    str_detect(x, "^>")    ~ paste0("vs_below", str_extract(x, "\\d+")),
    x == "nrpi"    ~ "nrpi_rank",
    TRUE ~ str_replace_all(x, "[^a-z0-9]+", "_")
  )
}

# Turn one standings sheet into a tidy tibble. Handles sheets with more than
# one header block (e.g. a conference split into divisions).
parse_standings <- function(m) {
  txt <- m$text
  hdr_rows <- which(apply(txt, 1, function(r) any(r == "RPI")))
  if (length(hdr_rows) == 0) return(NULL)
  
  updated <- txt[str_detect(txt[, 1], regex("^updated", ignore_case = TRUE)), 1]
  updated <- if (length(updated)) updated[1] else NA_character_
  
  block_ends <- c(hdr_rows[-1] - 1, nrow(txt))
  
  map2_dfr(hdr_rows, block_ends, function(h_row, end_row) {
    h <- txt[h_row, ]
    table_title <- h[1]
    h[1] <- "team"
    # carry group labels across blank header cells (W/L/T sub-columns)
    for (j in seq_along(h)[-1]) if (h[j] == "") h[j] <- h[j - 1]
    
    if (end_row <= h_row) return(NULL)
    body <- txt[(h_row + 1):end_row, , drop = FALSE]
    body <- body[body[, 1] != "" &
                   !str_detect(body[, 1], regex("^updated", ignore_case = TRUE)), , drop = FALSE]
    if (nrow(body) == 0) return(NULL)
    
    # drop spacer columns (empty in every team row)
    keep <- colSums(body != "") > 0
    keep[1] <- TRUE
    h <- h[keep]; body <- body[, keep, drop = FALSE]
    
    # name columns: 3-wide groups become _w/_l/_t
    r <- rle(h)
    nm <- unlist(map2(r$values, r$lengths, function(v, n) {
      base <- if (v == "team") "team" else clean_label(v)
      if (n == 1) base
      else if (n == 3) paste(base, c("w", "l", "t"), sep = "_")
      else paste(base, seq_len(n), sep = "_")
    }))
    nm <- make.unique(nm, sep = "_")
    nm[nm == "sos_rank_1"] <- "nrpi_sos_rank"   # second SOS column belongs to NRPI
    
    colnames(body) <- nm
    as_tibble(body, .name_repair = "minimal") %>%
      mutate(table_title = table_title, sheet_updated = updated, .before = 1)
  })
}

# ---- Step 1: read the archive grid ------------------------------------------

message("Reading archive index...")
idx_doc <- fetch_html(INDEX_URL)
if (is.null(idx_doc)) stop("Could not download the archive index sheet.")
idx <- parse_waffle(idx_doc)

hdr <- which(apply(idx$text, 1, function(r) "2019" %in% r))[1]
season_labels <- idx$text[hdr, ]

links <- map_dfr((hdr + 1):nrow(idx$text), function(i) {
  conf <- idx$text[i, 1]
  cols <- which(!is.na(idx$href[i, ]))
  if (conf == "" || length(cols) == 0) return(NULL)
  tibble(
    conference = conf,
    season     = season_labels[cols],
    post_url   = unwrap_google_link(idx$href[i, cols])
  )
})

# The archive has a few copy-paste errors (e.g. some Atlantic 10 cells point to
# Pac-12 posts; Independents point to WAC). When one post is linked from two
# conferences, keep the conference whose name actually appears in the URL.
links <- links %>%
  mutate(name_in_url = str_detect(post_url, fixed(slugify(str_remove(conference, " Conference$"))))) %>%
  arrange(desc(name_in_url)) %>%
  distinct(post_url, .keep_all = TRUE) %>%
  select(-name_in_url) %>%
  # "20-21" is the COVID season played in spring 2021; code it as 2020
  mutate(season_year = if_else(season == "20-21", 2020L, suppressWarnings(as.integer(season))))

if (!is.null(SEASONS))     links <- filter(links, season %in% SEASONS)
if (!is.null(CONFERENCES)) links <- filter(links, conference %in% CONFERENCES)

message(nrow(links), " conference-season pages to scrape.")

# ---- Step 2 + 3: open each post, find the embedded sheet, parse it ----------

scrape_one <- function(conference, season, season_year, post_url) {
  message("  ", season, " | ", conference)
  post <- fetch_html(post_url)
  if (is.null(post)) { warning("No post: ", post_url); return(NULL) }
  
  raw <- as.character(post)
  sheet_urls <- str_extract_all(raw, "https?://docs\\.google\\.com/spreadsheets?/[^\"'<>\\s]+")[[1]] %>%
    str_replace_all("&amp;", "&") %>%
    str_replace("^http://", "https://") %>%
    unique()
  sheet_urls <- sheet_urls[str_detect(sheet_urls, "pub")]
  if (length(sheet_urls) == 0) { warning("No embedded sheet in ", post_url); return(NULL) }
  
  map_dfr(sheet_urls, function(su) {
    sdoc <- fetch_html(su)
    if (is.null(sdoc)) return(NULL)
    m <- parse_waffle(sdoc)
    if (is.null(m)) return(NULL)
    parse_standings(m)
  }) %>%
    { if (nrow(.) == 0) NULL else mutate(., conference = conference, season = season,
                                         season_year = season_year, post_url = post_url, .before = 1) }
}

results <- pmap(links, safely(scrape_one))
errors  <- keep(results, ~ !is.null(.x$error))
rpi_raw <- map_dfr(results, "result")

if (length(errors)) message(length(errors), " pages errored; see `errors`.")

# ---- Step 4: tidy types and save --------------------------------------------

id_cols <- c("conference", "season", "season_year", "post_url",
             "table_title", "sheet_updated", "team")

rpi <- rpi_raw %>%
  mutate(across(-any_of(id_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
  relocate(any_of(c("season_year", "season", "conference", "team",
                    "rpi_rank", "sos_rank", "overall_w", "overall_l", "overall_t")))

write_csv(rpi, OUT_FILE)
message("Saved ", nrow(rpi), " team-seasons to ", OUT_FILE)

# ---- Quick checks -----------------------------------------------------------

# Which conference-seasons came back empty?
missing <- anti_join(links, distinct(rpi, conference, season), by = c("conference", "season"))
if (nrow(missing)) { message("No data for:"); print(missing, n = Inf) }

# Every season should have exactly one team at RPI #1
rpi %>% filter(rpi_rank == 1) %>% count(season) %>% print(n = Inf)

# ---- Examples for your project ----------------------------------------------

# Final RPI top 10 by season
top10 <- rpi %>%
  filter(!is.na(rpi_rank), rpi_rank <= 10) %>%
  arrange(desc(season_year), rpi_rank) %>%
  select(season, rpi_rank, team, conference, overall_w, overall_l, overall_t, sos_rank)
print(top10, n = 50)

# Virginia Tech's history
rpi %>%
  filter(team == "Virginia Tech") %>%
  arrange(season_year) %>%
  select(season, rpi_rank, sos_rank, overall_w, overall_l, overall_t, conf_pts) %>%
  print(n = Inf)