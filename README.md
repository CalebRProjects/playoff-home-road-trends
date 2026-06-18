# NBA Home-Court Advantage Trends

## Overview

This project analyzes how NBA home-court advantage has changed over time, with a specific focus on whether modern road teams have become harder to bury.

The analysis combines long-view home/road trends from 1950-2026 with modern playoff game-level data from 2002-2026. It looks at regular-season and playoff home win percentage, close playoff games, three-point attempt volume, late-round playoff splits, and the best modern playoff road teams.

## Main Question

Has NBA home-court advantage become less automatic, and where does that change show up most clearly?

## Key Findings

* NBA home-court advantage has declined across both the regular season and playoffs.
* Playoff home teams won 64.7% of games from 2002-2018, but only 57.6% from 2019-2026, excluding the 2020 bubble.
* Close playoff games have shifted sharply toward road teams in the modern period.
* The rise in three-point volume does not fully explain the decline, but modern shot profiles appear connected to road offense becoming less fragile.
* Late-round playoff games have played much closer to even in the current era.
* The best modern playoff teams are increasingly capable of maintaining their identity away from home.

## Data Sources

* Basketball Reference expanded standings
* ESPN schedules and team box scores
* `hoopR` package

Historical home/road records from 1950-2001 were collected from Basketball Reference expanded standings. Modern game-level data from 2002-2026 comes from ESPN schedules through `hoopR`.

## Tools Used

* R
* tidyverse
* hoopR
* rvest
* janitor
* ggplot2
* scales
* ggrepel
* zoo
* kableExtra
* Quarto / R Markdown

## Methodology

This analysis uses two levels of data.

Historical home/road records from 1950-2001 are season-level records collected from Basketball Reference expanded standings. These records are used for the long-view regular-season and playoff home-court trend charts.

Modern data from 2002-2026 is game-level data collected from ESPN schedules and team box scores through `hoopR`. This data is used for the modern playoff sections, including close games, round-level splits, three-point attempt trends, and road team rankings.

A 2002-2006 overlap check matched playoff games and home wins exactly across Basketball Reference and ESPN/hoopR sources. Regular-season overlap differed by one game in each checked season, with negligible percentage impact.

The 2020 bubble season is excluded from modern home/road comparisons because home and road designations did not reflect normal arena environments.

Close games are defined as playoff games decided by five or fewer points. This is not the same as official clutch-time data, but it provides a transparent game-level proxy that can be applied consistently across the dataset.

Three-point attempt rate is measured as playoff 3PA per team game.

Playoff rounds were inferred by series structure rather than ESPN note fields. Games were grouped by season and matchup, then each team’s series order was used to assign First Round, Conference Semifinals, Conference Finals, and NBA Finals labels.

## Key Outputs

* Long-view NBA home win percentage trend, 1950-2026
* NBA home win percentage by decade
* Close playoff home win percentage by era
* Playoff 3PA vs. home win percentage scatter plot
* Late-round playoff home/road results table
* Best playoff road teams since 2002

## File Guide

* `scripts/01_collect_playoff_games.R`
  Collects modern NBA schedule and team box score data from ESPN through `hoopR`.

* `scripts/01b_collect_bref_historical_home_road.R`
  Collects historical home/road records from Basketball Reference expanded standings.

* `scripts/02_build_home_road_summaries.R`
  Builds season, era, round, and team-level summaries used in the report.

* `scripts/03_create_visuals.R`
  Creates the final charts used in the report.

* `data/raw/`
  Stores raw downloaded data.

* `data/processed/`
  Stores cleaned and summarized datasets.

* `outputs/figures/`
  Stores final chart images.

* `outputs/tables/`
  Stores exported summary tables.

* `report/`
  Contains the final R Markdown report.

## Limitations

* Basketball Reference historical data is season-level, while modern ESPN/hoopR data is game-level.
* Close-game analysis uses final margin rather than possession-by-possession clutch data.
* The 2020 bubble is excluded from modern home/road comparisons because it did not reflect normal home-court conditions.
* Three-point volume is treated as one marker of modern offensive change, not a single causal explanation.
* Late-round playoff samples are smaller, so those splits should be interpreted directionally.

## Future Extensions

* Add true clutch-time data where available
* Compare road net rating by team
* Analyze series-level road win patterns
* Compare champion and non-champion road performance
* Add player-level or lineup-level road splits
* Explore travel distance, rest, altitude, or time-zone effects
