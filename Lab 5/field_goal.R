## Lab 5
## Field Goal probability

football_data2014 <- readRDS("pbp2014-2024.rds")

unique(football_data2014$field_goal_result)

library(dplyr)

fg_data <- football_data2014 %>% 
  filter(play_type == "field_goal") %>% 
  mutate(FG_made = ifelse(field_goal_result == "made", 1, 0)) #filter out
football_data2014$FG_made <- ifelse(football_data2014$field_goal_result == "made", 1, 0)
football_data2014$FP <- football_data2014$kick_distance
fg_model <- glm(FG_made ~ FP, family = binomial, data = football_data2014)
field_goal_probability <- function(FP) {
  log_odds <- predict(fg_model, newdata = data.frame(FP = FP))  
  prob <- exp(log_odds) / (1 + exp(log_odds))  #get probability
  return(unname(prob))
}

