
library(mixtools)
library(tidyverse)
library(stats)
library(ggplot2)

# Load your data
#football_data2014 <- read_rds("/Users/zhaoshibo/Desktop/STAT4800/pbp2014-2024.rds")

# Logistic model for field goal probability
football_data2014 <- football_data2014 %>%
  mutate(FG_made = ifelse(field_goal_result == "made", 1, 0))

# FG model (trained on actual FG attempts)
fg_data <- football_data2014 %>% 
  filter(play_type == "field_goal")

fg_model <- glm(FG_made ~ kick_distance, family = binomial, data = fg_data)

field_goal_probability <- function(kick_distance) {
  log_odds <- predict(fg_model, newdata = data.frame(kick_distance = kick_distance))
  prob <- exp(log_odds) / (1 + exp(log_odds))
  return(unname(prob))
}

# Create success model for 4th down
football_plays <- football_data2014 %>%
  filter(down == 4, play_type %in% c("run", "pass"),
         !is.na(ydstogo), !is.na(yardline_100), !is.na(yards_gained)) %>%
  mutate(success = ifelse(yards_gained >= ydstogo, 1, 0))

success_model <- glm(success ~ ydstogo + yardline_100, data = football_plays, family = binomial())

# Custom simulate_drive function for forced play
simulate_drive_custom <- function(state, forced_play) {
  if (forced_play == "go_for_it") {
    prob_success <- predict(success_model, newdata = data.frame(ydstogo = state$ytg, yardline_100 = state$fp), type = "response")
    success <- rbinom(1, 1, prob_success)
    
    if (success == 1) {
      state$fp <- min(state$fp + state$ytg, 100)
      state$down <- 1
      state$ytg <- 10
      return(simulate_drive(start_fp = state$fp))
    } else {
      opponent_fp <- 100 - state$fp
      result <- simulate_drive(start_fp = opponent_fp)
      result$points <- -result$points
      return(result)
    }
  } else if (forced_play == "punt") {
    return(list(points = -simulate_punt(state$fp)))
  } else if (forced_play == "field_goal") {
    return(list(points = attempt_field_goal(state$fp)))
  } else if (forced_play == "opponent_drive") {
    return(simulate_drive(start_fp = state$fp))
  }
}

# Simulate punt logic
simulate_punt <- function(fp) {
  punt_distance <- pmin(pmax(rnorm(1, 44, 5), 30), 60)
  if (runif(1) < 0.05) punt_distance <- runif(1, 20, 30)
  punted_fp <- fp - punt_distance
  opponent_fp <- ifelse(punted_fp < 0, 20, 100 - punted_fp)
  result <- simulate_drive(start_fp = opponent_fp)
  return(result$points)
}

# Field goal outcome
attempt_field_goal <- function(fp) {
  kick_distance <- 100 - fp + 17  
  prob <- field_goal_probability(kick_distance)
  made <- rbinom(1, 1, prob)
  return(ifelse(made == 1, 3, 0))
}

# Handle extra point after TD
handle_extra_points <- function(choice = "kick") {
  if (choice == "kick") {
    made <- rbinom(1, 1, 0.94)
    return(ifelse(made == 1, 1, 0))
  } else {
    made <- rbinom(1, 1, 0.5)
    return(ifelse(made == 1, 2, 0))
  }
}

# Simulate a full drive from a field position
simulate_drive <- function(start_fp = 25, after_td_choice = "kick") {
  state <- list(fp = start_fp, down = 1, ytg = 10, event = NA, points = 0)
  
  while (is.na(state$event) || state$event == "PLAY") {
    state <- run_drive(state)
  }
  
  if (state$event == "TOUCHDOWN") {
    extra <- handle_extra_points(after_td_choice)
    state$points <- 6 + extra
  } else if (state$event == "FIELD_GOAL_ATTEMPT") {
    state$points <- attempt_field_goal(state$fp)
  } else if (state$event == "SAFETY") {
    state$points <- -2
  } else {
    state$points <- 0
  }
  
  return(state)
}

# Logic to run plays
run_drive <- function(state) {
  max_gain <- 120 - state$fp
  min_gain <- -state$fp
  yard_change <- sample(min_gain:max_gain, 1)
  state$fp <- max(0, min(120, state$fp + yard_change))
  state$ytg <- max(0, state$ytg - yard_change)
  
  if (state$fp >= 100) {
    state$event <- "TOUCHDOWN"
    return(state)
  }
  if (state$fp <= 0) {
    state$event <- "SAFETY"
    return(state)
  }
  if (state$ytg <= 0) {
    state$down <- 1
    state$ytg <- 10
  } else {
    state$down <- state$down + 1
  }
  if (state$down > 4) {
    if (state$fp >= 60) {
      state$event <- "FIELD_GOAL_ATTEMPT"
    } else {
      state$event <- "TURNOVER_ON_DOWNS"
    }
    return(state)
  }
  state$event <- "PLAY"
  return(state)
}

# Estimate expected points from sim
simulate_ep <- function(play_type, FP, YTG, reps = 100) {
  results <- replicate(reps, {
    state <- list(fp = FP, down = 4, ytg = YTG, event = NA, points = 0)
    result <- simulate_drive_custom(state, forced_play = play_type)
    result$points
  })
  mean(results)
}

# Example simulation
FP <- 95
YTG <- 5
p_success <- predict(success_model, newdata = data.frame(ydstogo = YTG, yardline_100 = FP), type = "response")
ep_go <- simulate_ep("go_for_it", FP, YTG)
ep_fg <- replicate(100, attempt_field_goal(FP)) %>% mean()
ep_punt <- simulate_ep("punt", FP, YTG)
ep_fail <- simulate_ep("opponent_drive", FP = 100 - FP, YTG = 10)

# Expected value if going for it
ep_go_expected <- p_success * ep_go + (1 - p_success) * ep_fail

# Break-even success probability
p_star <- (ep_punt - ep_fail) / (ep_go - ep_fail)

# Print results
cat("EP (Go For It):", ep_go_expected, "\n")
cat("EP (Field Goal):", ep_fg, "\n")
cat("EP (Punt):", ep_punt, "\n")
cat("Estimated success probability (Go):", p_success, "\n")
cat("Break-even probability (Go):", round(p_star, 3), "\n")





run_decision_simulation <- function(FP, YTG, reps = 500) {
  p_success <- predict(success_model, newdata = data.frame(ydstogo = YTG, yardline_100 = FP), type = "response")
  ep_success <- mean(replicate(reps, simulate_drive(start_fp = min(FP + YTG, 100))$points))
  ep_fail <- mean(replicate(reps, simulate_drive(start_fp = 100 - FP)$points))
  ep_go <- p_success * ep_success - (1 - p_success) * ep_fail
  
  if (FP < 60) {
    ep_alt <- mean(replicate(reps, simulate_punt(FP)))
    alt_type <- "Punt"
  } else {
    kick_distance <- 100-FP+17
    prob <- field_goal_probability(kick_distance)
    ep_alt <- 3 * prob
    alt_type <- "Field Goal"
  }
  
  tibble(
    FP = FP,
    YTG = YTG,
    EP_Go_For_It = ep_go,
    EP_Alternative = ep_alt,
    Best_Decision = ifelse(ep_go > ep_alt, "Go For It", alt_type),
    P_Success = p_success
  )
}


scenarios <- expand.grid(
  FP = 1:99,       # exclude 0 and 100
  YTG = 1:10
) %>%
  filter(FP + YTG <= 100)
library(purrr)
library(dplyr)

results_full <- map2_dfr(scenarios$FP, scenarios$YTG, ~ run_decision_simulation(.x, .y, reps = 100))


ggplot(results_full, aes(x = FP, fill = Best_Decision)) +
  geom_bar(position = "stack") +
  facet_wrap(~ YTG, ncol = 2, drop = FALSE) +
  labs(title = "Best 4th Down Decision by Field Position and Yards to Go",
       x = "Field Position",
       y = "Count of Scenarios") +
  scale_fill_manual(
    values = c("Go For It" = "green", "Punt" = "blue", "Field Goal" = "orange"),
    drop = FALSE
  ) +
  theme_minimal()

