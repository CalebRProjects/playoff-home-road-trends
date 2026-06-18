# 01_collect_playoff_games.R

# Goal:
# Pull NBA playoff game-level results and team box scores from hoopR/ESPN,
# then build base home-road and 3PA datasets.

library(tidyverse)
library(hoopR)
library(janitor)
library(lubridate)
library(glue)
library(scales)

# -----------------------------
# Settings
# -----------------------------

start_year <- 2002
end_year <- 2026

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Helpers
# -----------------------------

safe_load_nba_schedule <- function(season_year) {
  message(glue("Loading NBA schedule: {season_year}"))
  
  tryCatch(
    hoopR::load_nba_schedule(seasons = season_year),
    error = function(e) {
      message(glue("Failed schedule for {season_year}: {e$message}"))
      tibble()
    }
  )
}

safe_load_nba_team_box <- function(season_year) {
  message(glue("Loading NBA team box: {season_year}"))
  
  tryCatch(
    hoopR::load_nba_team_box(seasons = season_year),
    error = function(e) {
      message(glue("Failed team box for {season_year}: {e$message}"))
      tibble()
    }
  )
}

get_col <- function(data, options) {
  hit <- options[options %in% names(data)][1]
  if (is.na(hit)) return(NA_character_)
  hit
}

# -----------------------------
# Pull schedules
# -----------------------------

nba_schedule_raw <- map_dfr(start_year:end_year, function(yr) {
  Sys.sleep(0.4)
  safe_load_nba_schedule(yr)
})

write_csv(nba_schedule_raw, glue("data/raw/nba_schedule_raw_{start_year}_{end_year}.csv"))

nba_schedule_clean <- nba_schedule_raw %>%
  clean_names()

all_completed_games <- nba_schedule_clean %>%
  filter(
    season_type %in% c(2, 3),
    status_type_completed == TRUE
  ) %>%
  transmute(
    season = season,
    season_type = season_type,
    season_type_label = case_when(
      season_type == 2 ~ "Regular Season",
      season_type == 3 ~ "Playoffs",
      TRUE ~ "Other"
    ),
    game_id = as.character(game_id),
    date = as_date(game_date),
    home_team = home_display_name,
    away_team = away_display_name,
    home_team_abbr = home_abbreviation,
    away_team_abbr = away_abbreviation,
    home_pts = as.numeric(home_score),
    away_pts = as.numeric(away_score),
    neutral_site = neutral_site,
    notes_type = notes_type,
    notes_headline = notes_headline,
    status_type_detail = status_type_detail
  ) %>%
  filter(
    !is.na(home_pts),
    !is.na(away_pts)
  ) %>%
  mutate(
    home_win = home_pts > away_pts,
    road_win = away_pts > home_pts,
    winner = if_else(home_win, home_team, away_team),
    loser = if_else(home_win, away_team, home_team),
    margin = abs(home_pts - away_pts),
    close_game = margin <= 5,
    bubble_year = season == 2020
  ) %>%
  arrange(season, season_type, date, game_id)

write_csv(all_completed_games, "data/processed/nba_completed_games_home_road.csv")

# -----------------------------
# Build playoff game dataset
# -----------------------------

playoff_games <- all_completed_games %>%
  filter(season_type_label == "Playoffs")

write_csv(playoff_games, "data/processed/playoff_games_home_road.csv")

# -----------------------------
# Pull team box scores
# -----------------------------

nba_team_box_raw <- map_dfr(start_year:end_year, function(yr) {
  Sys.sleep(0.4)
  safe_load_nba_team_box(yr)
})

write_csv(nba_team_box_raw, glue("data/raw/nba_team_box_raw_{start_year}_{end_year}.csv"))

team_box <- nba_team_box_raw %>%
  clean_names()

# -----------------------------
# Detect 3PA columns
# -----------------------------

season_col <- get_col(team_box, c("season", "season_year"))
game_id_col <- get_col(team_box, c("game_id", "id"))

team_col <- get_col(team_box, c(
  "team_display_name",
  "team_name",
  "team_short_display_name",
  "team"
))

team_abbr_col <- get_col(team_box, c(
  "team_abbreviation",
  "team_abbrev",
  "team_short_display_name"
))

three_pa_col <- get_col(team_box, c(
  "three_point_field_goals_attempted",
  "three_point_field_goal_attempts",
  "three_point_attempts",
  "three_pa",
  "fg3a",
  "field_goals_3pt_attempted"
))

if (any(is.na(c(season_col, game_id_col, team_col, three_pa_col)))) {
  cat("\nTeam box columns available:\n")
  names(team_box) %>%
    sort() %>%
    as_tibble_col(column_name = "column") %>%
    print(n = Inf)
  
  stop("Missing one or more needed team box columns. Check printed column names.")
}

# -----------------------------
# Keep playoff team box rows only
# -----------------------------

playoff_team_box <- team_box %>%
  mutate(
    game_id_join = as.character(.data[[game_id_col]]),
    season_join = as.integer(.data[[season_col]])
  ) %>%
  semi_join(
    playoff_games %>%
      transmute(
        season_join = as.integer(season),
        game_id_join = as.character(game_id)
      ),
    by = c("season_join", "game_id_join")
  ) %>%
  transmute(
    season = season_join,
    game_id = game_id_join,
    team = .data[[team_col]],
    team_abbr = if (!is.na(team_abbr_col)) .data[[team_abbr_col]] else .data[[team_col]],
    three_pa = as.numeric(.data[[three_pa_col]])
  ) %>%
  filter(!is.na(three_pa))

write_csv(playoff_team_box, "data/processed/playoff_team_box_playoffs.csv")

# -----------------------------
# Season-level 3PA + home-road summary
# -----------------------------

season_home_road <- playoff_games %>%
  group_by(season) %>%
  summarise(
    games = n(),
    home_wins = sum(home_win, na.rm = TRUE),
    road_wins = sum(road_win, na.rm = TRUE),
    home_win_pct = home_wins / games,
    road_win_pct = road_wins / games,
    close_games = sum(close_game, na.rm = TRUE),
    close_home_wins = sum(home_win & close_game, na.rm = TRUE),
    close_home_win_pct = close_home_wins / close_games,
    avg_margin = mean(margin, na.rm = TRUE),
    .groups = "drop"
  )

season_3pa <- playoff_team_box %>%
  group_by(season) %>%
  summarise(
    team_games = n(),
    playoff_3pa_per_team_game = mean(three_pa, na.rm = TRUE),
    .groups = "drop"
  )

home_road_3pa_by_season <- season_home_road %>%
  left_join(season_3pa, by = "season") %>%
  mutate(
    era = case_when(
      season < 2010 ~ "2002-2009",
      season < 2017 ~ "2010-2016",
      season < 2020 ~ "2017-2019",
      season == 2020 ~ "2020 Bubble",
      TRUE ~ "2021-2026"
    )
  )

write_csv(home_road_3pa_by_season, "data/processed/playoff_home_road_3pa_by_season.csv")

# -----------------------------
# Console checks
# -----------------------------

cat("\nPlayoff games saved:", nrow(playoff_games), "\n")
cat("Playoff team box rows saved:", nrow(playoff_team_box), "\n\n")

playoff_games %>%
  count(season) %>%
  print(n = Inf)

cat("\n3PA by season:\n")

home_road_3pa_by_season %>%
  select(season, games, home_win_pct, playoff_3pa_per_team_game) %>%
  mutate(home_win_pct = percent(home_win_pct, accuracy = 0.1)) %>%
  print(n = Inf)