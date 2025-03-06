# Load necessary libraries
library(tidyverse)

# Load the data
nhl_data <- read_csv("nhl_pbp20162017.csv")


# filter for shots
shots_data <- nhl_data %>%
  filter(!is.na(Type))


# point differential (for the team taking the shot)
shots_data <- shots_data %>%
  mutate(
    point_diff = if_else(
      Ev_Team == Home_Team,
      Home_Score - Away_Score,
      Away_Score - Home_Score
    )
  )


# time blocks
shots_data <- shots_data %>%
  mutate(
    time_block = case_when(
      Seconds_Elapsed %% 1200 <= 300 ~ "Block1",   # first 5 minutes
      Seconds_Elapsed %% 1200 <= 600 ~ "Block2",   # 5-10 minutes
      Seconds_Elapsed %% 1200 <= 900 ~ "Block3",   # 10-15 minutes
      TRUE ~ "Block4"                             # 15-20 minutes
    ),
    time_block = factor(time_block, levels = c("Block1", "Block2", "Block3", "Block4")),
    period = factor(Period)
  )


# subset for shot counts per team per game per period and time block
shot_counts <- shots_data %>%
  group_by(Game_Id, Ev_Team, period, time_block, point_diff) %>%
  summarise(
    shots = n(),
    .groups = "drop"
  )


# Fit Poisson model
shot_model <- glm(
  shots ~ time_block + period + point_diff,
  family = poisson,
  data = shot_counts
)

summary(shot_model)

