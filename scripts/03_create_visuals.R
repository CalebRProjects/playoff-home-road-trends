# 03_create_visuals.R

library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

season_home_road <- read_csv("data/processed/playoff_home_road_by_season.csv") %>%
  clean_names()

era_summary <- read_csv("data/processed/playoff_home_road_by_era.csv") %>%
  clean_names()

best_road_teams <- read_csv("data/processed/best_playoff_road_teams.csv") %>%
  clean_names()

home_road_3pa <- read_csv("data/processed/playoff_home_road_3pa_by_season.csv") %>%
  clean_names()

theme_caleb <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# -----------------------------
# 1. Home win percentage over time
# -----------------------------

p_home_trend <- season_home_road %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = home_win_pct), alpha = 0.35) +
  geom_line(aes(y = home_win_pct_5yr), linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 2017, linetype = "dotted", alpha = 0.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.45, 0.75)) +
  labs(
    title = "Playoff Home-Court Advantage Has Softened",
    subtitle = "NBA playoff home win percentage by season, with 5-year rolling average",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: hoopR / ESPN schedules | 2020 bubble season included but should be interpreted separately"
  ) +
  theme_caleb

ggsave(
  "outputs/figures/playoff_home_win_pct_over_time.png",
  p_home_trend,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------
# 2. Home win percentage by era
# -----------------------------

p_era <- era_summary %>%
  mutate(
    era = factor(era, levels = c("2002-2009", "2010-2016", "2017-2019", "2020 Bubble", "2021-2026"))
  ) %>%
  ggplot(aes(x = era, y = home_win_pct)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = percent(home_win_pct, accuracy = 0.1)),
    vjust = -0.4,
    size = 3.6
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.75)) +
  labs(
    title = "The Home Edge Has Dropped in the Modern Playoffs",
    subtitle = "NBA playoff home win percentage by era",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: hoopR / ESPN schedules"
  ) +
  theme_caleb

ggsave(
  "outputs/figures/playoff_home_win_pct_by_era.png",
  p_era,
  width = 7,
  height = 5,
  dpi = 300
)

# -----------------------------
# 3. Close-game home win percentage
# -----------------------------

p_close <- season_home_road %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = close_home_win_pct), alpha = 0.35) +
  geom_line(aes(y = close_home_win_pct_5yr), linewidth = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 2017, linetype = "dotted", alpha = 0.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.15, 0.85)) +
  labs(
    title = "Close Games Complicate the Home-Court Story",
    subtitle = "Home win percentage in playoff games decided by 5 or fewer points",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: hoopR / ESPN schedules | Close game defined by final margin of 5 or fewer"
  ) +
  theme_caleb

ggsave(
  "outputs/figures/close_playoff_home_win_pct_over_time.png",
  p_close,
  width = 8,
  height = 5,
  dpi = 300
)

# -----------------------------
# 4. Best road playoff teams
# -----------------------------

p_best_road <- best_road_teams %>%
  slice_max(road_wins, n = 15, with_ties = FALSE) %>%
  mutate(
    team_label = paste0(team_abbr, " ", season),
    team_label = fct_reorder(team_label, road_wins)
  ) %>%
  ggplot(aes(x = team_label, y = road_wins)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(road_wins, "-", road_losses)),
    hjust = -0.15,
    size = 3.3
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "The Best Playoff Road Teams Since 2002",
    subtitle = "Team-seasons ranked by road playoff wins, minimum 6 road games",
    x = NULL,
    y = "Road playoff wins",
    caption = "Data: hoopR / ESPN schedules"
  ) +
  theme_caleb

ggsave(
  "outputs/figures/best_playoff_road_teams_since_2002.png",
  p_best_road,
  width = 8,
  height = 5.5,
  dpi = 300
)

cat("Saved first visual set.\n")

# -----------------------------
# 5. Is it as simple as 3PA?
# -----------------------------

p_3pa_scatter <- home_road_3pa %>%
  filter(!is.na(playoff_3pa_per_team_game)) %>%
  ggplot(aes(x = playoff_3pa_per_team_game, y = home_win_pct)) +
  geom_point(aes(shape = era), size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.1) +
  geom_text_repel(
    data = home_road_3pa %>%
      filter(season %in% c(2002, 2008, 2016, 2018, 2020, 2026)),
    aes(label = season),
    size = 3.3,
    max.overlaps = Inf
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.45, 0.76)
  ) +
  scale_x_continuous(
    breaks = seq(15, 40, 5)
  ) +
  labs(
    title = "Is It as Simple as More Threes?",
    subtitle = "Playoff 3-point attempts per team game vs. home win percentage by season",
    x = "Playoff 3PA per team game",
    y = "Home win percentage",
    caption = "Data: hoopR / ESPN schedules and team box scores"
  ) +
  theme_caleb

ggsave(
  "outputs/figures/playoff_3pa_vs_home_win_pct.png",
  p_3pa_scatter,
  width = 8,
  height = 5,
  dpi = 300
)