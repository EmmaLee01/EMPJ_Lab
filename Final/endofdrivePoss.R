
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

## Punts

library(tidyverse)

set.seed(42)
n <- 1000

# Helper function to simulate outcomes for each zone
simulate_punts <- function(n, mean_dist, sd_dist, touchback_prob, zone_label) {
  punt_distance <- rnorm(n, mean = mean_dist, sd = sd_dist)
  punt_distance <- pmax(punt_distance, 0)
  
  # Determine if touchback occurs
  touchback <- rbinom(n, size = 1, prob = touchback_prob)
  
  # If not a touchback, simulate return or fair catch
  return_type <- ifelse(touchback == 1, "Touchback",
                        ifelse(punt_distance > 40,
                               ifelse(runif(length(punt_distance)) < 0.7, "Return", "Fair Catch"),
                               "Fair Catch"))
  
  tibble(
    zone = zone_label,
    punt_distance = punt_distance,
    touchback = touchback,
    result = return_type
  )
}

# Simulate each zone
punts_0_40 <- simulate_punts(n, 46, 6, 0.02, "0–40")
punts_40_50 <- simulate_punts(n, 42, 5, 0.05, "40–50")
punts_50_60 <- simulate_punts(n, 38, 4, 0.15, "50–60")

# Combine all into one dataframe
punt_data <- bind_rows(punts_0_40, punts_40_50, punts_50_60)

# Quick view of result frequencies
table(punt_data$zone, punt_data$result)

ggplot(punt_data, aes(x = punt_distance, fill = result)) +
  geom_histogram(binwidth = 1, position = "stack") +
  labs(title = "Distribution of Return Types by Punt Distance",
       x = "Punt Distance (yards)", y = "Count") +
  theme_minimal()

## Return data ###


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










