library(tidyverse)
library(mixtools)
footballdata <- readRDS('/Users/zhaoshibo/Desktop/STAT4800/pbp2014-2024.rds')


fourth_down_plays <- footballdata %>%
  filter(
    down == 4,
    play_type %in% c("run", "pass"),
    !is.na(yards_gained),
    !is.na(ydstogo),
    !is.na(yardline_100)
  ) %>%
  mutate(success = if_else(yards_gained >= ydstogo, 1, 0))



attempt_field_goal <- function(fp) {
  kick_distance <- 100 - fp + 17
  prob <- field_goal_probability(kick_distance)
  made <- rbinom(1, 1, prob)
  return(ifelse(made == 1, 3, 0))
}


run_drive <- function(state) {
  # Simulate yard gain (still basic for now)
  max_gain <- 120 - state$fp
  min_gain <- -state$fp
  yard_change <- sample(min_gain:max_gain, 1)
  
  # Update field position
  state$fp <- state$fp + yard_change
  state$fp <- max(0, min(120, state$fp))
  state$ytg <- max(0, state$ytg - yard_change)
  
  # Touchdown
  if (state$fp >= 100) {
    state$event <- "TOUCHDOWN"
    return(state)
  }
  
  # Safety
  if (state$fp <= 0) {
    state$event <- "SAFETY"
    return(state)
  }
  
  # First down?
  if (state$ytg <= 0) {
    state$down <- 1
    state$ytg <- 10
  } else {
    state$down <- state$down + 1
  }
  
  # If 4th down, check FG option
  if (state$down > 4) {
    if (state$fp >= 60) {
      state$event <- "FIELD_GOAL_ATTEMPT"
    } else {
      state$event <- "TURNOVER_ON_DOWNS"
    }
    return(state)
  }
  
  # Otherwise, continue playing
  state$event <- "PLAY"
  return(state)
}



handle_extra_points <- function(choice = "kick") {
  if (choice == "kick") {
    made <- rbinom(1, 1, 0.94)  # 94% success rate
    return(ifelse(made == 1, 1, 0))  # 1 point or 0
  } else if (choice == "2pt") {
    made <- rbinom(1, 1, 0.5)   # 50% success rate
    return(ifelse(made == 1, 2, 0))  # 2 points or 0
  }
}



simulate_drive <- function(start_fp = 25, after_td_choice = "kick") {
  state <- list(
    fp = start_fp,
    down = 1,
    ytg = 10,
    event = NA,
    points = 0
  )
  
  while (is.na(state$event) || state$event == "PLAY") {
    state <- run_drive(state)
  }
  
  # Handle scoring
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




set.seed(123)  # for reproducibility
test_result <- simulate_drive(start_fp = 75, after_td_choice = "kick")
print(test_result)
