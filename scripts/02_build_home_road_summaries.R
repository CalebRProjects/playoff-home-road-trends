# 02_build_home_road_summaries.R

library(tidyverse)
library(janitor)
library(slider)
library(scales)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

all_completed_games <- read_csv("data/processed/nba_completed_games_home_road.csv") %>%
  clean_names()

playoff_games <- read_csv("data/processed/playoff_games_home_road.csv") %>%
  clean_names()

# -----------------------------
# Infer playoff rounds by team series order
# -----------------------------

series_lookup <- playoff_games %>%
  mutate(
    team_1 = pmin(home_team, away_team),
    team_2 = pmax(home_team, away_team),
    series_id = paste(season, team_1, team_2, sep = "_")
  ) %>%
  group_by(season, series_id, team_1, team_2) %>%
  summarise(
    series_start = min(date, na.rm = TRUE),
    series_end = max(date, na.rm = TRUE),
    games_in_series = n(),
    .groups = "drop"
  )

team_series_order <- series_lookup %>%
  select(season, series_id, series_start, team_1, team_2) %>%
  pivot_longer(
    cols = c(team_1, team_2),
    names_to = "team_slot",
    values_to = "team"
  ) %>%
  arrange(season, team, series_start) %>%
  group_by(season, team) %>%
  mutate(team_round_number = row_number()) %>%
  ungroup()

series_rounds <- team_series_order %>%
  group_by(season, series_id) %>%
  summarise(
    round_number = max(team_round_number),
    .groups = "drop"
  ) %>%
  mutate(
    round = case_when(
      round_number == 1 ~ "First Round",
      round_number == 2 ~ "Conference Semifinals",
      round_number == 3 ~ "Conference Finals",
      round_number == 4 ~ "NBA Finals",
      TRUE ~ "Unknown"
    ),
    round = factor(
      round,
      levels = c(
        "First Round",
        "Conference Semifinals",
        "Conference Finals",
        "NBA Finals",
        "Unknown"
      )
    )
  )

playoff_games <- playoff_games %>%
  mutate(
    team_1 = pmin(home_team, away_team),
    team_2 = pmax(home_team, away_team),
    series_id = paste(season, team_1, team_2, sep = "_")
  ) %>%
  left_join(series_rounds, by = c("season", "series_id"))

# -----------------------------
# Regular season vs playoffs
# -----------------------------

season_type_home_road <- all_completed_games %>%
  group_by(season, season_type_label) %>%
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
  ) %>%
  arrange(season, season_type_label)

write_csv(
  season_type_home_road,
  "data/processed/nba_home_road_by_season_type.csv"
)

home_road_gap <- season_type_home_road %>%
  select(season, season_type_label, home_win_pct) %>%
  pivot_wider(
    names_from = season_type_label,
    values_from = home_win_pct
  ) %>%
  clean_names() %>%
  mutate(
    playoff_minus_regular = playoffs - regular_season
  )

write_csv(
  home_road_gap,
  "data/processed/nba_home_road_regular_vs_playoffs_gap.csv"
)

# -----------------------------
# Season-level home/road trends
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
    close_road_wins = sum(road_win & close_game, na.rm = TRUE),
    close_home_win_pct = close_home_wins / close_games,
    close_road_win_pct = close_road_wins / close_games,
    avg_margin = mean(margin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(season) %>%
  mutate(
    home_win_pct_3yr = slide_dbl(
      home_win_pct,
      mean,
      .before = 2,
      .complete = FALSE,
      na.rm = TRUE
    ),
    home_win_pct_5yr = slide_dbl(
      home_win_pct,
      mean,
      .before = 4,
      .complete = FALSE,
      na.rm = TRUE
    ),
    close_home_win_pct_5yr = slide_dbl(
      close_home_win_pct,
      mean,
      .before = 4,
      .complete = FALSE,
      na.rm = TRUE
    )
  )

write_csv(season_home_road, "data/processed/playoff_home_road_by_season.csv")

# -----------------------------
# Era summaries
# -----------------------------

era_summary <- playoff_games %>%
  mutate(
    era = case_when(
      season < 2010 ~ "2002-2009",
      season < 2017 ~ "2010-2016",
      season < 2020 ~ "2017-2019",
      season == 2020 ~ "2020 Bubble",
      TRUE ~ "2021-2026"
    )
  ) %>%
  group_by(era) %>%
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
  ) %>%
  mutate(
    era = factor(
      era,
      levels = c("2002-2009", "2010-2016", "2017-2019", "2020 Bubble", "2021-2026")
    )
  ) %>%
  arrange(era)

write_csv(era_summary, "data/processed/playoff_home_road_by_era.csv")

# -----------------------------
# Home/road by playoff round
# -----------------------------

round_home_road <- playoff_games %>%
  filter(round != "Unknown") %>%
  group_by(season, round) %>%
  summarise(
    games = n(),
    home_wins = sum(home_win, na.rm = TRUE),
    road_wins = sum(road_win, na.rm = TRUE),
    home_win_pct = home_wins / games,
    road_win_pct = road_wins / games,
    close_games = sum(close_game, na.rm = TRUE),
    close_home_wins = sum(home_win & close_game, na.rm = TRUE),
    close_home_win_pct = if_else(close_games > 0, close_home_wins / close_games, NA_real_),
    avg_margin = mean(margin, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(round_home_road, "data/processed/playoff_home_road_by_round.csv")

round_era_home_road <- playoff_games %>%
  filter(round != "Unknown") %>%
  mutate(
    era = case_when(
      season < 2010 ~ "2002-2009",
      season < 2019 ~ "2010-2018",
      season == 2020 ~ "2020 Bubble",
      season >= 2019 & season != 2020 ~ "2019-2026*"
    ),
    era = factor(
      era,
      levels = c("2002-2009", "2010-2018", "2019-2026*", "2020 Bubble")
    )
  ) %>%
  group_by(era, round) %>%
  summarise(
    games = n(),
    home_wins = sum(home_win, na.rm = TRUE),
    road_wins = sum(road_win, na.rm = TRUE),
    home_win_pct = home_wins / games,
    road_win_pct = road_wins / games,
    close_games = sum(close_game, na.rm = TRUE),
    close_home_wins = sum(home_win & close_game, na.rm = TRUE),
    close_home_win_pct = if_else(close_games > 0, close_home_wins / close_games, NA_real_),
    avg_margin = mean(margin, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(round_era_home_road, "data/processed/playoff_home_road_by_round_era.csv")

round_era_home_road %>%
  mutate(
    home_win_pct = scales::percent(home_win_pct, accuracy = 0.1),
    road_win_pct = scales::percent(road_win_pct, accuracy = 0.1),
    close_home_win_pct = scales::percent(close_home_win_pct, accuracy = 0.1)
  ) %>%
  arrange(round, era) %>%
  print(n = Inf)

# -----------------------------
# Best playoff road teams
# -----------------------------

team_road_games <- playoff_games %>%
  transmute(
    season,
    team = away_team,
    team_abbr = away_team_abbr,
    opponent = home_team,
    road_pts = away_pts,
    opp_pts = home_pts,
    road_win,
    margin = road_pts - opp_pts,
    close_game
  )

best_road_teams <- team_road_games %>%
  group_by(season, team, team_abbr) %>%
  summarise(
    road_games = n(),
    road_wins = sum(road_win, na.rm = TRUE),
    road_losses = road_games - road_wins,
    road_win_pct = road_wins / road_games,
    road_point_diff = sum(margin, na.rm = TRUE),
    road_avg_margin = mean(margin, na.rm = TRUE),
    close_road_games = sum(close_game, na.rm = TRUE),
    close_road_wins = sum(road_win & close_game, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(road_games >= 6) %>%
  arrange(desc(road_wins), desc(road_win_pct), desc(road_avg_margin))

write_csv(best_road_teams, "data/processed/best_playoff_road_teams.csv")

# -----------------------------
# Console checks
# -----------------------------

cat("\nRegular season vs playoffs:\n")

season_type_home_road %>%
  filter(season >= 2017) %>%
  mutate(
    home_win_pct = percent(home_win_pct, accuracy = 0.1),
    road_win_pct = percent(road_win_pct, accuracy = 0.1),
    close_home_win_pct = percent(close_home_win_pct, accuracy = 0.1)
  ) %>%
  print(n = Inf)

cat("\nPlayoff vs regular-season home-court gap:\n")

home_road_gap %>%
  filter(season >= 2017) %>%
  mutate(
    regular_season = percent(regular_season, accuracy = 0.1),
    playoffs = percent(playoffs, accuracy = 0.1),
    playoff_minus_regular = percent(playoff_minus_regular, accuracy = 0.1)
  ) %>%
  print(n = Inf)

cat("\nSeason summary:\n")
season_home_road %>%
  select(season, games, home_win_pct, road_win_pct, close_games, close_home_win_pct) %>%
  tail(10) %>%
  mutate(
    home_win_pct = percent(home_win_pct, accuracy = 0.1),
    road_win_pct = percent(road_win_pct, accuracy = 0.1),
    close_home_win_pct = percent(close_home_win_pct, accuracy = 0.1)
  ) %>%
  print(n = 10)

cat("\nEra summary:\n")
era_summary %>%
  mutate(
    home_win_pct = percent(home_win_pct, accuracy = 0.1),
    road_win_pct = percent(road_win_pct, accuracy = 0.1),
    close_home_win_pct = percent(close_home_win_pct, accuracy = 0.1)
  ) %>%
  print(n = Inf)

cat("\nRound era summary:\n")

round_era_home_road %>%
  mutate(
    home_win_pct = percent(home_win_pct, accuracy = 0.1),
    road_win_pct = percent(road_win_pct, accuracy = 0.1),
    close_home_win_pct = percent(close_home_win_pct, accuracy = 0.1)
  ) %>%
  arrange(round, era) %>%
  print(n = Inf)

cat("\nUnknown round count:\n")

playoff_games %>%
  count(season, round) %>%
  filter(round == "Unknown") %>%
  print(n = Inf)

cat("\nBest road teams:\n")
best_road_teams %>%
  select(season, team, road_wins, road_losses, road_win_pct, road_avg_margin) %>%
  mutate(road_win_pct = percent(road_win_pct, accuracy = 0.1)) %>%
  head(20) %>%
  print(n = 20)