# 03_create_visuals.R

library(tidyverse)
library(janitor)
library(scales)
library(ggrepel)
library(zoo)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

season_home_road <- read_csv("data/processed/playoff_home_road_by_season.csv") %>%
  clean_names()

era_summary <- read_csv("data/processed/playoff_home_road_by_era.csv") %>%
  clean_names()

best_road_teams <- read_csv("data/processed/best_playoff_road_teams.csv") %>%
  clean_names()

home_road_3pa <- read_csv("data/processed/playoff_home_road_3pa_by_season.csv") %>%
  clean_names()

BG <- "#F6F4EF"
TEXT_DARK <- "#151515"
GRID <- "#DDD8CF"

theme_caleb_elevated <- function() {
  theme_minimal(base_family = "Inter", base_size = 12) +
    theme(
      plot.background = element_rect(fill = BG, color = NA),
      panel.background = element_rect(fill = BG, color = NA),
      panel.grid.major.x = element_line(color = GRID, linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0.5,
        color = TEXT_DARK,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        color = "#333333",
        margin = margin(b = 12)
      ),
      axis.title.x = element_text(
        size = 12,
        face = "bold",
        color = TEXT_DARK,
        margin = margin(t = 8)
      ),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 10, color = "#4A4A4A"),
      axis.text.y = element_text(size = 11, face = "bold", color = "#4A4A4A"),
      legend.position = "none",
      plot.caption = element_text(
        size = 8.5,
        color = "#666666",
        hjust = 1,
        margin = margin(t = 8)
      ),
      plot.margin = margin(12, 32, 14, 14)
    )
}

# -----------------------------
# 1. Home win percentage over time
# -----------------------------

all_home_road_by_season <- read_csv(
  "data/processed/nba_home_road_by_season_1950_2026.csv",
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    season = as.integer(season),
    season_type_label = as.character(season_type_label),
    home_win_pct = as.numeric(home_win_pct)
  )

home_trend_long <- all_home_road_by_season %>%
  filter(season_type_label %in% c("Regular Season", "Playoffs")) %>%
  arrange(season_type_label, season) %>%
  group_by(season_type_label) %>%
  mutate(
    home_win_pct_3yr = zoo::rollapply(
      home_win_pct,
      width = 3,
      FUN = mean,
      fill = NA,
      align = "right",
      partial = TRUE,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

label_points <- home_trend_long %>%
  group_by(season_type_label) %>%
  filter(season == max(season, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    label = case_when(
      season_type_label == "Regular Season" ~ "Regular\nSeason",
      TRUE ~ season_type_label
    ),
    label_y = case_when(
      season_type_label == "Playoffs" ~ home_win_pct_3yr + 0.010,
      season_type_label == "Regular Season" ~ home_win_pct_3yr - 0.010,
      TRUE ~ home_win_pct_3yr
    ),
    label_x = season + 1.4
  )

p_home_trend <- home_trend_long %>%
  ggplot(aes(x = season)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.45
  ) +
  geom_line(
    aes(y = home_win_pct, group = season_type_label),
    color = "#C8C8C8",
    linewidth = 0.35,
    alpha = 0.34
  ) +
  geom_line(
    aes(
      y = home_win_pct_3yr,
      color = season_type_label
    ),
    linewidth = 1.45
  ) +
  geom_text(
    data = label_points,
    aes(
      x = label_x,
      y = label_y,
      label = label,
      color = season_type_label
    ),
    size = 3.9,
    fontface = "bold",
    hjust = 0,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Regular Season" = "#2F4156",
      "Playoffs" = "#9E2A2B"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.45, 0.80),
    breaks = seq(0.50, 0.80, 0.10)
  ) +
  scale_x_continuous(
    breaks = seq(1950, 2025, 10),
    limits = c(1950, 2035)
  ) +
  labs(
    title = "Home Court Has Become Less Automatic",
    subtitle = "Regular-season and playoff home win percentage since 1950, shown as 3-year rolling averages",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: Basketball Reference / ESPN.com | Viz: @Rambzee_"
  ) +
  theme_caleb_elevated() +
  theme(
    panel.grid.major.x = element_line(color = GRID, linewidth = 0.35),
    panel.grid.major.y = element_line(color = GRID, linewidth = 0.35),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      color = TEXT_DARK,
      angle = 90,
      vjust = 0.5,
      margin = margin(r = 8)
    ),
    legend.position = "none",
    plot.margin = margin(12, 58, 14, 18)
  )

ggsave(
  "outputs/figures/nba_home_win_pct_over_time_1950_2026.png",
  p_home_trend,
  width = 9.2,
  height = 5.3,
  dpi = 300,
  bg = BG
)

# -----------------------------
# 2. Home win percentage by decade
# -----------------------------

home_by_decade <- all_home_road_by_season %>%
  filter(season_type_label %in% c("Regular Season", "Playoffs")) %>%
  filter(season != 2020) %>%
  mutate(
    decade = case_when(
      season >= 2020 ~ "2020-2026*",
      TRUE ~ paste0(floor(season / 10) * 10, "s")
    ),
    decade = factor(
      decade,
      levels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020-2026*")
    )
  ) %>%
  group_by(decade, season_type_label) %>%
  summarise(
    games = sum(games, na.rm = TRUE),
    home_wins = sum(home_wins, na.rm = TRUE),
    home_win_pct = home_wins / games,
    .groups = "drop"
  )

p_home_decade <- home_by_decade %>%
  ggplot(aes(x = decade, y = home_win_pct, fill = season_type_label)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.45
  ) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = percent(home_win_pct, accuracy = 0.1)),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 3.1,
    fontface = "bold",
    color = TEXT_DARK
  ) +
  scale_fill_manual(
    values = c(
      "Playoffs" = "#9E2A2B",
      "Regular Season" = "#2F4156"
    ),
    breaks = c("Playoffs", "Regular Season")
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(alpha = 0.95)
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.82),
    breaks = seq(0, 0.80, 0.20)
  ) +
  labs(
    title = "The NBA’s Home Edge Has Fallen by Decade",
    subtitle = "Regular-season and playoff home win percentage by decade",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: Basketball Reference / ESPN.com | *2020 bubble excluded | Viz: @Rambzee_"
  ) +
  theme_caleb_elevated() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = GRID, linewidth = 0.35),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      color = TEXT_DARK,
      angle = 90,
      vjust = 0.5,
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(size = 10, face = "bold", color = "#4A4A4A"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 9.5, face = "bold", color = TEXT_DARK),
    legend.key.size = unit(0.42, "cm"),
    legend.spacing.x = unit(0.35, "cm"),
    legend.margin = margin(t = -2, b = -6),
    legend.box.margin = margin(t = -8, b = -8),
    plot.margin = margin(12, 24, 14, 18)
  )

ggsave(
  "outputs/figures/nba_home_win_pct_by_decade_1950_2026.png",
  p_home_decade,
  width = 9.2,
  height = 5.3,
  dpi = 300,
  bg = BG
)

# -----------------------------
# 3. Close-game home win percentage by era
# -----------------------------

close_era_plot <- season_home_road %>%
  filter(season != 2020) %>%
  mutate(
    era = case_when(
      season < 2010 ~ "2002-2009",
      season < 2019 ~ "2010-2018",
      season >= 2019 ~ "2019-2026*"
    ),
    era = factor(
      era,
      levels = c("2002-2009", "2010-2018", "2019-2026*")
    )
  )

close_era_summary <- close_era_plot %>%
  group_by(era) %>%
  summarise(
    close_games = sum(close_games, na.rm = TRUE),
    close_home_wins = sum(close_home_wins, na.rm = TRUE),
    close_home_win_pct = close_home_wins / close_games,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(percent(close_home_win_pct, accuracy = 0.1), "\n", close_games, " games"),
    fill_group = if_else(close_home_win_pct >= 0.5, "Home edge", "Road edge")
  )

p_close_era <- ggplot(close_era_summary, aes(x = era, y = close_home_win_pct)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.45
  ) +
  geom_col(
    aes(fill = fill_group),
    width = 0.64,
    alpha = 0.96
  ) +
  geom_point(
    data = close_era_plot,
    aes(x = era, y = close_home_win_pct),
    inherit.aes = FALSE,
    position = position_jitter(width = 0.10, height = 0),
    size = 1.8,
    alpha = 0.38,
    color = "#1F1F1F"
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 3.4,
    fontface = "bold",
    color = TEXT_DARK,
    lineheight = 0.92
  ) +
  scale_fill_manual(
    values = c(
      "Home edge" = "#007A33",
      "Road edge" = "#9E2A2B"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.80),
    breaks = seq(0, 0.75, 0.25)
  ) +
  labs(
    title = "Close Playoff Games Have Shifted Toward Road Teams",
    subtitle = "Home win percentage in playoff games decided by five or fewer points",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: ESPN.com | *2020 bubble excluded | Dots represent individual seasons | Viz: @Rambzee_"
  ) +
  theme_caleb_elevated() +
  theme(
    plot.title = element_text(size = 19, face = "bold", hjust = 0.5, color = TEXT_DARK),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "#333333", margin = margin(b = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = GRID, linewidth = 0.35),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      color = TEXT_DARK,
      angle = 90,
      vjust = 0.5,
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(size = 10.5, face = "bold", color = "#4A4A4A"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#4A4A4A"),
    legend.position = "none",
    plot.margin = margin(12, 22, 14, 22)
  )

ggsave(
  "outputs/figures/close_playoff_home_win_pct_by_era.png",
  p_close_era,
  width = 9.2,
  height = 5.2,
  dpi = 300,
  bg = BG
)

# -----------------------------
# 4. Best road playoff teams by win percentage
# -----------------------------

team_colors <- tribble(
  ~team_abbr, ~team_color,
  "ATL", "#E03A3E",
  "BOS", "#007A33",
  "BKN", "#000000",
  "CHA", "#1D1160",
  "CHI", "#CE1141",
  "CLE", "#860038",
  "DAL", "#00538C",
  "DEN", "#0E2240",
  "DET", "#C8102E",
  "GS",  "#1D428A",
  "GSW", "#1D428A",
  "HOU", "#CE1141",
  "IND", "#002D62",
  "LAC", "#C8102E",
  "LAL", "#552583",
  "MEM", "#5D76A9",
  "MIA", "#98002E",
  "MIL", "#00471B",
  "MIN", "#0C2340",
  "NO",  "#0C2340",
  "NOP", "#0C2340",
  "NY",  "#006BB6",
  "NYK", "#006BB6",
  "OKC", "#007AC1",
  "ORL", "#0077C0",
  "PHI", "#006BB6",
  "PHX", "#1D1160",
  "POR", "#E03A3E",
  "SA",  "#8A8F93",
  "SAS", "#8A8F93",
  "SAC", "#5A2D81",
  "TOR", "#CE1141",
  "UTA", "#002B5C",
  "WAS", "#002B5C",
  "WSH", "#002B5C"
)

team_abbr_clean <- tribble(
  ~team_abbr, ~team_abbr_plot,
  "NY",  "NYK",
  "GS",  "GSW",
  "SA",  "SAS",
  "WSH", "WAS"
)

best_road_pct_plot <- best_road_teams %>%
  filter(
    season != 2020,
    road_games >= 6
  ) %>%
  arrange(desc(road_win_pct), desc(road_wins), desc(road_avg_margin)) %>%
  slice_head(n = 10) %>%
  left_join(team_abbr_clean, by = "team_abbr") %>%
  mutate(
    team_abbr_plot = coalesce(team_abbr_plot, team_abbr)
  ) %>%
  left_join(team_colors, by = c("team_abbr_plot" = "team_abbr")) %>%
  mutate(
    team_color = coalesce(team_color, "#5A5A5A"),
    team_label = paste0(team_abbr_plot, " ", season),
    record_label = paste0(road_wins, "-", road_losses),
    pct_label = percent(road_win_pct, accuracy = 0.1),
    label = paste0(pct_label, "  (", record_label, ")"),
    team_label = fct_reorder(team_label, road_win_pct)
  )

p_best_road_pct <- ggplot(
  best_road_pct_plot,
  aes(x = team_label, y = road_win_pct, fill = team_color)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = label),
    hjust = -0.08,
    size = 3.7,
    fontface = "bold",
    color = TEXT_DARK
  ) +
  coord_flip() +
  scale_fill_identity() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 0.9, 0.3),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.13))
  ) +
  labs(
    title = "The Best Playoff Road Teams Since 2002",
    subtitle = "Team-seasons ranked by road win percentage | Min. 6 road games | 2020 excluded",
    x = NULL,
    y = "Road playoff win percentage",
    caption = "Data: ESPN.com | 2020 bubble excluded | Viz: @Rambzee_"
  ) +
  theme_caleb_elevated()

ggsave(
  "outputs/figures/best_playoff_road_teams_by_win_pct_since_2002.png",
  p_best_road_pct,
  width = 8.5,
  height = 5.4,
  dpi = 300,
  bg = BG
)

# -----------------------------
# 5. The three-point boom and home-court shift
# -----------------------------

three_pa_seasons <- home_road_3pa %>%
  filter(
    !is.na(playoff_3pa_per_team_game),
    season != 2020
  ) %>%
  mutate(
    era = case_when(
      season < 2010 ~ "2002-2009",
      season < 2019 ~ "2010-2018",
      season >= 2019 ~ "2019-2026*"
    ),
    era = factor(
      era,
      levels = c("2002-2009", "2010-2018", "2019-2026*")
    ),
    season_label = if_else(
      season %in% c(2008, 2018, 2026),
      as.character(season),
      NA_character_
    )
  )

p_3pa_era <- ggplot(
  three_pa_seasons,
  aes(x = playoff_3pa_per_team_game, y = home_win_pct)
) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.45
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = TEXT_DARK,
    linewidth = 0.9,
    alpha = 0.85
  ) + 
  geom_point(
    aes(fill = era),
    shape = 21,
    size = 3.8,
    stroke = 0.6,
    color = TEXT_DARK,
    alpha = 0.95
  ) +
  ggrepel::geom_text_repel(
    aes(label = season_label),
    size = 3.4,
    fontface = "bold",
    color = TEXT_DARK,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    na.rm = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "2002-2009" = "#2F4156",
      "2010-2018" = "#6D7F95",
      "2019-2026*" = "#9E2A2B"
    )
  ) +
  scale_x_continuous(
    breaks = seq(15, 40, 5),
    limits = c(14, 38)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.48, 0.76),
    breaks = seq(0.50, 0.75, 0.05)
  ) +
  labs(
    title = "The Three-Point Era Has Narrowed the Home Edge",
    subtitle = "Higher-volume playoff 3PA seasons sit closer to a neutral home-road split",
    x = "Playoff 3PA per team game",
    y = "Home win percentage",
    fill = NULL,
    caption = "Data: ESPN.com | *Excludes 2020 bubble | Viz: @Rambzee_"
  ) +
  theme_caleb_elevated() +
  theme(
    panel.grid.major.x = element_line(color = GRID, linewidth = 0.35),
    panel.grid.major.y = element_line(color = GRID, linewidth = 0.35),
    axis.title.x = element_text(
      size = 11,
      face = "bold",
      color = TEXT_DARK,
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size = 11,
      face = "bold",
      color = TEXT_DARK,
      angle = 90,
      vjust = 0.5,
      margin = margin(r = 8)
    ),
    axis.text.x = element_text(size = 10, face = "bold", color = "#4A4A4A"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#4A4A4A"),
    legend.position = "top",
    legend.text = element_text(size = 10, face = "bold", color = TEXT_DARK),
    plot.margin = margin(12, 28, 14, 18)
  )

ggsave(
  "outputs/figures/playoff_3pa_home_win_shift_by_era.png",
  p_3pa_era,
  width = 8.8,
  height = 5.3,
  dpi = 300,
  bg = BG
)
