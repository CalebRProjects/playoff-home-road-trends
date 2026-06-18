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

p_home_trend <- season_home_road %>%
  ggplot(aes(x = season)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.45
  ) +
  geom_line(
    aes(y = home_win_pct),
    color = "#A9A9A9",
    linewidth = 0.85,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = home_win_pct_5yr),
    color = TEXT_DARK,
    linewidth = 1.65
  ) +
  geom_point(
    data = season_home_road %>%
      filter(season %in% c(2008, 2020, 2026)),
    aes(y = home_win_pct),
    color = TEXT_DARK,
    size = 2.8
  ) +
  ggrepel::geom_text_repel(
    data = season_home_road %>%
      filter(season %in% c(2008, 2020, 2026)),
    aes(y = home_win_pct, label = season),
    size = 3.5,
    fontface = "bold",
    color = TEXT_DARK,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.45, 0.76),
    breaks = seq(0.50, 0.75, 0.05)
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2025, 5)
  ) +
  labs(
    title = "Playoff Home-Court Advantage Has Softened",
    subtitle = "NBA playoff home win percentage by season, with five-year rolling average",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: ESPN.com | Viz: @Rambzee_"
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
    axis.text.x = element_text(size = 10.5, face = "bold", color = "#4A4A4A"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#4A4A4A"),
    plot.margin = margin(12, 24, 14, 18)
  )

ggsave(
  "outputs/figures/playoff_home_win_pct_over_time.png",
  p_home_trend,
  width = 9,
  height = 5.3,
  dpi = 300,
  bg = BG
)

# -----------------------------
# 2. Home win percentage by era
# -----------------------------

era_summary_clean <- season_home_road %>%
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
  ) %>%
  group_by(era) %>%
  summarise(
    games = sum(games, na.rm = TRUE),
    home_wins = sum(home_wins, na.rm = TRUE),
    road_wins = sum(road_wins, na.rm = TRUE),
    home_win_pct = home_wins / games,
    road_win_pct = road_wins / games,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(percent(home_win_pct, accuracy = 0.1), "\n", games, " games"),
    fill_group = case_when(
      era == "2019-2026*" ~ "Modern",
      TRUE ~ "Earlier"
    )
  )

p_era <- ggplot(era_summary_clean, aes(x = era, y = home_win_pct)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "#777777",
    linewidth = 0.45
  ) +
  geom_col(
    aes(fill = fill_group),
    width = 0.62,
    alpha = 0.96
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.35,
    size = 3.6,
    fontface = "bold",
    color = TEXT_DARK,
    lineheight = 0.92
  ) +
  scale_fill_manual(
    values = c(
      "Earlier" = "#2F4156",
      "Modern" = "#9E2A2B"
    )
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.75),
    breaks = seq(0, 0.75, 0.25)
  ) +
  labs(
    title = "The Playoff Home Edge Has Dropped in the Parity Era",
    subtitle = "NBA playoff home win percentage by period",
    x = NULL,
    y = "Home win percentage",
    caption = "Data: ESPN.com | *Excludes 2020 bubble | Viz: @Rambzee_"
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
    axis.text.x = element_text(size = 11, face = "bold", color = "#4A4A4A"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#4A4A4A"),
    legend.position = "none",
    plot.margin = margin(12, 24, 14, 18)
  )

ggsave(
  "outputs/figures/playoff_home_win_pct_by_era.png",
  p_era,
  width = 8.5,
  height = 5.2,
  dpi = 300,
  bg = BG
)

# -----------------------------
# 3. Close-game home win percentage by era
# -----------------------------

close_era_plot <- season_home_road %>%
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
    caption = "Data: ESPN.com | *Excludes 2020 bubble | Dots represent individual seasons | Viz: @Rambzee_"
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
  filter(road_games >= 6) %>%
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
    subtitle = "Team-seasons ranked by road win percentage | Min. 6 road games",
    x = NULL,
    y = "Road playoff win percentage",
    caption = "Data: ESPN.com | Viz: @Rambzee_"
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

three_pa_era <- home_road_3pa %>%
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
    )
  ) %>%
  group_by(era) %>%
  summarise(
    seasons = n(),
    games = sum(games, na.rm = TRUE),
    home_wins = sum(home_wins, na.rm = TRUE),
    home_win_pct = home_wins / games,
    playoff_3pa_per_team_game = mean(playoff_3pa_per_team_game, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    three_label = paste0(round(playoff_3pa_per_team_game, 1), " 3PA"),
    home_label = percent(home_win_pct, accuracy = 0.1)
  )

# Scale home win% onto the 3PA axis for a clean two-metric display.
home_min <- 0.50
home_max <- 0.68
three_min <- 15
three_max <- 36

three_pa_era <- three_pa_era %>%
  mutate(
    home_scaled = scales::rescale(
      home_win_pct,
      to = c(three_min, three_max),
      from = c(home_min, home_max)
    )
  )

three_pa_era <- three_pa_era %>%
  mutate(
    home_label_y = case_when(
      era == "2019-2026*" ~ home_scaled + 2.7,
      TRUE ~ home_scaled + 2.0
    )
  )

p_3pa_era <- ggplot(three_pa_era, aes(x = era)) +
  geom_col(
    aes(y = playoff_3pa_per_team_game),
    width = 0.58,
    fill = "#2F4156",
    alpha = 0.94
  ) +
  geom_line(
    aes(y = home_scaled, group = 1),
    color = TEXT_DARK,
    linewidth = 1.35
  ) +
  geom_point(
    aes(y = home_scaled),
    color = TEXT_DARK,
    size = 3.6
  ) +
  geom_text(
    aes(y = playoff_3pa_per_team_game, label = three_label),
    vjust = -0.45,
    size = 3.6,
    fontface = "bold",
    color = "#2F4156"
  ) +
  geom_text(
    aes(y = home_label_y, label = home_label),
    size = 3.6,
    fontface = "bold",
    color = TEXT_DARK
  ) +
  scale_y_continuous(
    name = "Playoff 3PA",
    limits = c(0, 40),
    breaks = seq(0, 40, 10),
    sec.axis = sec_axis(
      trans = ~ scales::rescale(
        .,
        to = c(home_min, home_max),
        from = c(three_min, three_max)
      ),
      labels = percent_format(accuracy = 1),
      name = "Home win %"
    )
  ) +
  labs(
    title = "More Threes Are Only Part of the Home-Court Story",
    subtitle = "Playoff 3PA rose sharply, but the home-court shift likely reflects more than shot diet",
    x = NULL,
    caption = "Data: ESPN.com | *Excludes 2020 bubble | Viz: @Rambzee_"
  ) +
  theme_caleb_elevated() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = GRID, linewidth = 0.35),
    axis.title.y.left = element_text(
      size = 11,
      face = "bold",
      color = "#2F4156",
      angle = 90,
      vjust = 0.5,
      margin = margin(r = 8)
    ),
    axis.title.y.right = element_text(
      size = 11,
      face = "bold",
      color = TEXT_DARK,
      angle = 90,
      vjust = 0.5,
      margin = margin(l = 8)
    ),
    axis.text.x = element_text(size = 11, face = "bold", color = "#4A4A4A"),
    axis.text.y = element_text(size = 10, face = "bold", color = "#4A4A4A"),
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