library(dplyr)
pbp_data <- readRDS('/Users/zhaoshibo/Desktop/STAT4800/pbp2014-2024.rds')
fg_data <- pbp_data %>% 
  filter(play_type == "field_goal") %>% 
  mutate(FG_made = ifelse(field_goal_result == "made", 1, 0)) #filter out
pbp_data$FG_made <- ifelse(pbp_data$field_goal_result == "made", 1, 0)
pbp_data$FP <- pbp_data$kick_distance
fg_model <- glm(FG_made ~ FP, family = binomial, data = pbp_data)
field_goal_probability <- function(FP) {
  log_odds <- predict(fg_model, newdata = data.frame(FP = FP))  
  prob <- exp(log_odds) / (1 + exp(log_odds))  #get probability
  return(unname(prob))
}

down_four <- function(D, YTG, FP) {
  
  if (FP >= 80 & FP <= 100) {  # Opponent’s red zone
    play_probs <- c("go_for_it" = 0.231, "field_goal" = 0.717, "punt" = 0.052)  
  } else if (FP >= 60 & FP < 80) {  # Opponent’s territory
    play_probs <- c("go_for_it" = 0.251, "field_goal" = 0.589, "punt" = 0.089)  
  } else if (FP >= 40 & FP < 60) {  # Midfield
    play_probs <- c("go_for_it" = 0.178, "field_goal" = 0.005, "punt" = 0.747)  
  } else {  # Own side of the field (1-40)
    play_probs <- c("go_for_it" = 0.064, "field_goal" = 0.000, "punt" = 0.897) 
  }
  
  
  # Sample a play type
  play_type <- sample(names(play_probs), size = 1, prob = play_probs)
  print(paste("Play selected:", play_type)) 
  
  if (play_type == "go_for_it") {
    YG <- sample(0:YTG, 1)  # Simulated yards gained
    FP <- FP + YG
    new_YTG <- max(YTG - YG, 0)
    
    if (new_YTG == 0) {
      exit_drive <- 0  # First down achieved
    } else {
      exit_drive <- 1  # Turnover on downs
    }
    
    YTG <- new_YTG
    D <- 1  # Reset down after conversion/turnover
    
  } else if (play_type == "punt") {
    punt_distance <- sample(35:50, 1)  # Typical punt range
    FP <- max(FP - punt_distance, 0)
    YTG <- 10
    D <- 1
    exit_drive <- 1  # Change possession
    
  } else if (play_type == "field_goal") {
    FG_prob <- field_goal_probability(FP)  # Get probability from logistic model
    FG_made <- rbinom(1, 1, FG_prob)  # Simulate FG attempt
    
    if (FG_made == 1) {
      FP <- 115
    } 
    
    YTG <- 10
    D <- 1
    exit_drive <- 1  # End possession after FG attempt
  }
  
  list(D = D, YTG = YTG, FP = FP, exit_drive = exit_drive)
}
}