# scripts/01b_collect_historical_home_road.R

library(tidyverse)
library(rvest)
library(janitor)
library(glue)
library(stringr)

# -----------------------------
# Helpers
# -----------------------------

parse_record <- function(x) {
  x <- as.character(x)
  
  tibble(record = x) %>%
    mutate(
      wins = as.integer(str_extract(record, "^\\d+")),
      losses = as.integer(str_extract(record, "(?<=-)\\d+$")),
      games = wins + losses
    )
}

read_expanded_standings <- function(url, season, season_type_label) {
  message(glue("Reading {season_type_label} {season}: {url}"))
  
  page_text <- readLines(url, warn = FALSE)
  page_text <- paste(page_text, collapse = "\n")
  
  # Basketball Reference often stores tables inside HTML comments
  page_text <- str_replace_all(page_text, "<!--", "")
  page_text <- str_replace_all(page_text, "-->", "")
  
  page <- read_html(page_text)
  
  table_node <- page %>%
    html_element("#expanded_standings")
  
  if (length(table_node) == 0 || is.na(table_node)) {
    table_ids <- page %>%
      html_elements("table") %>%
      html_attr("id")
    
    stop(glue(
      "Could not find #expanded_standings for {season_type_label} {season}. ",
      "Available table IDs: {paste(table_ids, collapse = ', ')}"
    ))
  }
  
  expanded_raw <- table_node %>%
    html_table(fill = TRUE)
  
  if (
    is.null(names(expanded_raw)) ||
    !all(c("Team", "Overall", "Home", "Road") %in% names(expanded_raw))
  ) {
    header_row <- which(apply(expanded_raw, 1, function(row) {
      all(c("Team", "Overall", "Home", "Road") %in% as.character(row))
    }))[1]
    
    if (is.na(header_row)) {
      stop(glue("Could not identify header row for {season_type_label} {season}."))
    }
    
    names(expanded_raw) <- make.unique(as.character(expanded_raw[header_row, ]))
    expanded_raw <- expanded_raw[-seq_len(header_row), ]
  }
  
  expanded <- expanded_raw %>%
    clean_names() %>%
    filter(
      !is.na(team),
      team != "Team",
      !str_detect(team, "Division|Conference")
    )
  
  home_records <- parse_record(expanded$home)
  road_records <- parse_record(expanded$road)
  overall_records <- parse_record(expanded$overall)
  
  expanded %>%
    transmute(
      season = as.integer(season),
      season_type_label = season_type_label,
      team = team,
      overall = overall,
      home = home,
      road = road,
      overall_wins = overall_records$wins,
      overall_losses = overall_records$losses,
      overall_games = overall_records$games,
      home_wins = home_records$wins,
      home_losses = home_records$losses,
      home_games = home_records$games,
      road_wins = road_records$wins,
      road_losses = road_records$losses,
      road_games = road_records$games
    )
}

read_bref_home_road_season <- function(season) {
  regular_url <- glue("https://www.basketball-reference.com/leagues/NBA_{season}_standings.html")
  playoff_url <- glue("https://www.basketball-reference.com/playoffs/NBA_{season}_standings.html")
  
  regular <- read_expanded_standings(
    url = regular_url,
    season = season,
    season_type_label = "Regular Season"
  )
  
  # Small pause between regular season and playoff page
  Sys.sleep(5)
  
  playoffs <- read_expanded_standings(
    url = playoff_url,
    season = season,
    season_type_label = "Playoffs"
  )
  
  bind_rows(regular, playoffs)
}

# -----------------------------
# Run scraper
# -----------------------------

# TEST FIRST:
# seasons <- 2000:2001

# Full historical extension:
seasons <- 1950:2001

results <- map(seasons, function(season) {
  message(glue("\nStarting season {season}"))
  
  result <- safely(read_bref_home_road_season)(season)
  
  if (!is.null(result$error)) {
    message(glue("FAILED season {season}: {result$error$message}"))
  } else {
    message(glue("SUCCESS season {season}"))
  }
  
  message(glue("Finished season {season}. Sleeping 60 seconds..."))
  Sys.sleep(60)
  
  result
})

# -----------------------------
# Check errors
# -----------------------------

errors <- results %>%
  map("error") %>%
  compact()

if (length(errors) > 0) {
  message("Some seasons failed:")
  print(errors)
} else {
  message("All seasons scraped successfully.")
}

# -----------------------------
# Bind successful results
# -----------------------------

historical_team_home_road <- results %>%
  map("result") %>%
  compact() %>%
  bind_rows()

if (nrow(historical_team_home_road) == 0) {
  stop("No historical data was collected. Check errors above.")
}

historical_team_home_road %>%
  count(season, season_type_label) %>%
  print(n = Inf)

write_csv(
  historical_team_home_road,
  "data/processed/historical_team_home_road_1950_2001.csv"
)

# -----------------------------
# Season-level summary
# -----------------------------

historical_home_road_by_season <- historical_team_home_road %>%
  group_by(season, season_type_label) %>%
  summarise(
    teams = n(),
    games = sum(home_games, na.rm = TRUE),
    home_wins = sum(home_wins, na.rm = TRUE),
    home_losses = sum(home_losses, na.rm = TRUE),
    road_wins = sum(road_wins, na.rm = TRUE),
    road_losses = sum(road_losses, na.rm = TRUE),
    home_win_pct = home_wins / games,
    road_win_pct = road_wins / games,
    .groups = "drop"
  )

write_csv(
  historical_home_road_by_season,
  "data/processed/historical_home_road_by_season_1950_2001.csv"
)

historical_home_road_by_season %>%
  mutate(
    home_win_pct = scales::percent(home_win_pct, accuracy = 0.1),
    road_win_pct = scales::percent(road_win_pct, accuracy = 0.1)
  ) %>%
  print(n = Inf)

# -----------------------------
# Verification checks
# -----------------------------

verification_checks <- historical_home_road_by_season %>%
  mutate(
    home_road_loss_match = home_wins == road_losses,
    road_home_loss_match = road_wins == home_losses,
    home_games_match = games == home_wins + home_losses,
    road_games_match = games == road_wins + road_losses,
    total_result_match = home_wins + road_wins == games,
    pct_match = abs(home_win_pct - (home_wins / games)) < 0.00001
  )

verification_checks %>%
  summarise(
    rows = n(),
    failed_home_road_loss_match = sum(!home_road_loss_match, na.rm = TRUE),
    failed_road_home_loss_match = sum(!road_home_loss_match, na.rm = TRUE),
    failed_home_games_match = sum(!home_games_match, na.rm = TRUE),
    failed_road_games_match = sum(!road_games_match, na.rm = TRUE),
    failed_total_result_match = sum(!total_result_match, na.rm = TRUE),
    failed_pct_match = sum(!pct_match, na.rm = TRUE)
  )

historical_home_road_by_season %>%
  count(season) %>%
  filter(n != 2)

historical_team_home_road %>%
  filter(
    season %in% c(1986, 1996, 2001),
    season_type_label == "Playoffs"
  ) %>%
  select(
    season,
    team,
    overall,
    home,
    road,
    overall_wins,
    overall_losses,
    home_wins,
    home_losses,
    road_wins,
    road_losses
  ) %>%
  arrange(season, desc(overall_wins))

home_by_decade %>%
  arrange(decade, season_type_label) %>%
  mutate(
    home_win_pct = percent(home_win_pct, accuracy = 0.1)
  ) %>%
  print(n = Inf)

# -----------------------------
# Combine historical BRef + modern hoopR summaries
# -----------------------------

modern_home_road_by_season <- read_csv(
  "data/processed/nba_home_road_by_season_type.csv"
) %>%
  clean_names() %>%
  select(
    season,
    season_type_label,
    games,
    home_wins,
    road_wins,
    home_win_pct,
    road_win_pct
  ) %>%
  mutate(
    source = "ESPN / hoopR"
  )

historical_home_road_by_season_clean <- historical_home_road_by_season %>%
  mutate(
    source = "Basketball Reference"
  ) %>%
  select(
    season,
    season_type_label,
    games,
    home_wins,
    road_wins,
    home_win_pct,
    road_win_pct,
    source
  )

all_home_road_by_season <- bind_rows(
  historical_home_road_by_season_clean,
  modern_home_road_by_season
) %>%
  arrange(season, season_type_label)

write_csv(
  all_home_road_by_season,
  "data/processed/nba_home_road_by_season_1950_2026.csv"
)