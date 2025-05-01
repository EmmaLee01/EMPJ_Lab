
### Project scratch work

## Question - Deviating from highest expected points play: why & when, 
## what do you do instead?

## Look at just fourth down or all downs?
## Have simulation run with all downs but then specifically look at 4th
## Decision threshold (p) where it makes sense, EP x p(success) vs ep x p(fail)
## 

## What factors do we want to look into? - Things beyond data?

## football_data2014 <- readRDS("pbp2014-2024.rds")

library(dplyr)

# Field Goal

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

## function from logistic regression file

## Punts & returns - gives final field position for next drive

library(ggplot2)
library(dplyr)

simulate_punts <- function(n, fp, mean_dist, sd_dist, zone_label) {
  punt_distance <- rnorm(n, mean = mean_dist, sd = sd_dist)
  punt_distance <- pmax(punt_distance, 0)
  
  # Raw landing spot from receiving team's end zone
  raw_landing <- fp - punt_distance
  
  # Touchback if punt lands beyond the end zone
  touchback <- raw_landing < 0
  
  # Return type based only on distance and no random touchback chance
  return_type <- ifelse(touchback, "Touchback",
                        ifelse(punt_distance > 40,
                               ifelse(runif(n) < 0.7, "Return", "Fair Catch"),
                               "Fair Catch"))
  
  # Return yards only if it's a return
  return_yards <- ifelse(return_type == "Return", pmax(rnorm(n, mean = 7, sd = 1.5), 0), 0)
  
  # Final field position
  final_position <- ifelse(touchback, 20,
                           pmin(pmax(raw_landing + return_yards, 1), 99))
  
}


## Interceptions & Fumbles

combined_pass_run_summary <- football_data2014 %>%
  filter(play_type %in% c("run", "pass")) %>%
  mutate(
    yrdln_clean = as.numeric(str_extract(yrdln, "\\d+")),
    red_zone = ifelse(str_detect(yrdln, defteam) & yrdln_clean <= 20, "Red Zone", "Non-Red Zone")
  ) %>%
  group_by(red_zone, play_type) %>%
  summarise(
    total_plays = n(),
    avg_yards = mean(yards_gained, na.rm = TRUE),
    
    # For passes: Incomplete, Complete, Interception
    incompletions = sum(incomplete_pass == 1, na.rm = TRUE),
    completions = sum(complete_pass == 1, na.rm = TRUE),
    interceptions = sum(interception == 1, na.rm = TRUE),
    
    # For runs: Fumbles
    fumbles = sum(fumble_lost == 1, na.rm = TRUE)
  ) %>%
  # Add fumble percentage for runs
  mutate(
    fumble_pct = ifelse(play_type == "run", fumbles / total_plays * 100, NA),
    pass_completion_pct = ifelse(play_type == "pass", completions / total_plays * 100, NA),
    pass_incompletion_pct = ifelse(play_type == "pass", incompletions / total_plays * 100, NA),
    interception_pct = ifelse(play_type == "pass", interceptions / total_plays * 100, NA)
  ) %>%
  select(
    red_zone, play_type, total_plays, avg_yards,
    incompletions, completions, interceptions, fumbles,
    pass_completion_pct, pass_incompletion_pct, interception_pct, fumble_pct
  )

# Display combined summary
print(combined_pass_run_summary)










