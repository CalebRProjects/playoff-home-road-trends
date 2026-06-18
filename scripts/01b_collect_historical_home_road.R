# 01b_collect_historical_home_road.R

# Goal:
# Placeholder script for historical NBA home/road results before hoopR/ESPN coverage.
#
# Current project split:
# - 2002-2026: full modern analysis using hoopR/ESPN
# - 1970-2001: future schedule-only historical extension
#
# This script will eventually create:
# data/processed/historical_home_road_1970_2001.csv
#
# Expected columns:
# season
# season_type_label
# date
# home_team
# away_team
# home_pts
# away_pts
# home_win
# road_win
# margin
# close_game
#
# Notes:
# - Do not run Basketball Reference scraping aggressively.
# - Use slow requests and cache every season.
# - Historical extension should only support the long-view home/road trend.
# - Keep 3PA, round splits, best road teams, and team box-score analysis in the 2002-2026 hoopR/ESPN dataset.

library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)
library(glue)

# -----------------------------
# Settings
# -----------------------------

start_year <- 1970
end_year <- 2001

dir.create("data/raw/historical", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Future helpers
# -----------------------------

get_bref_schedule_url <- function(season_end_year) {
  # Basketball Reference season schedule page.
  # Example: 2001 season -> NBA_2001_games.html
  glue("https://www.basketball-reference.com/leagues/NBA_{season_end_year}_games.html")
}

read_cached_or_download <- function(url, cache_path) {
  if (file.exists(cache_path)) {
    message("Reading cached file: ", cache_path)
    return(read_html(cache_path))
  }
  
  message("Downloading: ", url)
  
  # Keep this very slow if/when activated.
  Sys.sleep(runif(1, 10, 20))
  
  page <- tryCatch(
    read_html(url),
    error = function(e) {
      message("Failed: ", e$message)
      return(NULL)
    }
  )
  
  if (!is.null(page)) {
    writeLines(as.character(page), cache_path)
  }
  
  page
}

# -----------------------------
# Historical collection placeholder
# -----------------------------

# IMPORTANT:
# Leave this disabled until ready.
# Basketball Reference recently returned a 429 rate limit, so do not run this loop yet.

historical_games <- tibble(
  season = integer(),
  season_type_label = character(),
  date = as.Date(character()),
  home_team = character(),
  away_team = character(),
  home_pts = numeric(),
  away_pts = numeric(),
  home_win = logical(),
  road_win = logical(),
  margin = numeric(),
  close_game = logical()
)

# write_csv(
#   historical_games,
#   "data/processed/historical_home_road_1970_2001.csv"
# )

cat("Historical shell created. Do not run scraping until source/rate-limit plan is finalized.\n")