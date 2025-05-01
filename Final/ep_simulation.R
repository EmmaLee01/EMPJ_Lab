library(tidyverse)
library(mixtools)

# Load data
football_data2014 <- readRDS("pbp2014-2024.rds")

# --- Field Goal Model ---
football_data2014 <- football_data2014 %>%
  mutate(FG_made = ifelse(field_goal_result == "made", 1, 0),
         FP = kick_distance)

fg_model <- glm(FG_made ~ FP, family = binomial, data = football_data2014)

field_goal_probability <- function(FP) {
  log_odds <- predict(fg_model, newdata = data.frame(FP = FP))
  prob <- exp(log_odds) / (1 + exp(log_odds))
  return(unname(prob))
}

attempt_field_goal <- function(fp) {
  prob <- field_goal_probability(fp)
  made <- rbinom(1, 1, prob)
  return(ifelse(made == 1, 3, 0))
}

handle_extra_points <- function(choice = "kick") {
  if (choice == "kick") {
    made <- rbinom(1, 1, 0.94)
    return(ifelse(made == 1, 1, 0))
  } else {
    made <- rbinom(1, 1, 0.5)
    return(ifelse(made == 1, 2, 0))
  }
}

# --- Placeholder yardage model (replace with your actual mix_model setup) ---
sample_yards <- function(n, model) {
  rnorm(n, mean = 5, sd = 3)
}

mix_model <- NULL  # replace with your trained EM model if available

# --- Run Play Dispatcher ---
run_play <- function(down, ytg, fp) {
  if (down == 1) {
    return(down_one(down, ytg, fp))
  } else if (down == 2) {
    return(down_two(down, ytg, fp))
  } else if (down == 3) {
    return(down_three(down, ytg, fp))
  } else {
    return(down_four(down, ytg, fp))
  }
}

down_one <- function(down, ytg, fp) {
  gain <- round(sample_yards(1, mix_model))
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)
  if (new_ytg <= 0) list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  else list(down = 2, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}

down_two <- function(down, ytg, fp) {
  gain <- round(sample_yards(1, mix_model))
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)
  if (new_ytg <= 0) list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  else list(down = 3, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}

down_three <- function(down, ytg, fp) {
  gain <- round(sample_yards(1, mix_model))
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)
  if (new_ytg <= 0) list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  else list(down = 4, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}

fthdpredict <- function(FP, YTG) {
  if (FP >= 90) {
    probs <- c(go_for_it = 0.6, field_goal = 0.35, punt = 0.05)
  } else if (FP >= 60) {
    probs <- c(go_for_it = 0.4, field_goal = 0.4, punt = 0.2)
  } else if (FP >= 40) {
    probs <- c(go_for_it = 0.3, field_goal = 0.2, punt = 0.5)
  } else {
    probs <- c(go_for_it = 0.1, field_goal = 0.1, punt = 0.8)
  }
  return(probs)
}


down_four <- function(D, YTG, FP) {
  play_probs <- fthdpredict(FP, YTG)
  play_type <- sample(names(play_probs), size = 1, prob = play_probs)
  
  if (play_type == "go_for_it") {
    YG <- round(sample_yards(1, mix_model))
    YG <- max(YG, 0)
    FP <- FP + YG
    new_YTG <- max(YTG - YG, 0)
    if (new_YTG == 0) exit_drive <- 0 else exit_drive <- 1
    YTG <- new_YTG
    D <- 1
  } else if (play_type == "punt") {
    FP <- max(FP - sample(35:50, 1), 0)
    YTG <- 10
    D <- 1
    exit_drive <- 1
  } else if (play_type == "field_goal") {
    FG_made <- rbinom(1, 1, field_goal_probability(FP))
    if (FG_made == 1) FP <- 115
    YTG <- 10
    D <- 1
    exit_drive <- 1
  }
  
  list(down = D, ytg = YTG, fp = FP, exit_drive = exit_drive)
}

# --- Drive Simulation using run_play() ---
run_drive <- function(state) {
  while (TRUE) {
    result <- run_play(state$down, state$ytg, state$fp)
    state$down <- result$down
    state$ytg <- result$ytg
    state$fp <- result$fp
    
    if (state$fp >= 100) {
      state$event <- "TOUCHDOWN"
      state$points <- 6 + handle_extra_points("kick")
      break
    } else if (result$exit_drive == 1) {
      state$event <- "END_DRIVE"
      state$points <- if (state$fp >= 60) attempt_field_goal(state$fp) else 0
      break
    }
  }
  return(state)
}

simulate_drive <- function(start_fp = 25) {
  state <- list(fp = start_fp, down = 1, ytg = 10, event = NA, points = 0)
  state <- run_drive(state)
  return(state)
}

# --- Epoch simulation ---
run_epoch <- function(state) {
  sim <- simulate_drive(start_fp = state$fp)
  return(sim$points)
}

top_func <- function(downs = 1, ytg = 10, fp = NULL, epochs = 1000) {
  scores <- numeric(epochs)
  for (i in 1:epochs) {
    state <- list(
      down = downs,
      ytg = ytg,
      fp = if (is.null(fp)) sample(1:100, 1) else fp
    )
    scores[i] <- run_epoch(state)
  }
  mean(scores)
}

# Run example
set.seed(42)
ep_result <- top_func(downs = 4, ytg = 2, fp = 50, epochs = 1000)
cat("Estimated Expected Points:", ep_result, "\n")
