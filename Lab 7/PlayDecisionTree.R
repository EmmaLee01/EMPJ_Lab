## Yards gained mixture model
## Lab 7

# football_data2014 <- readRDS("pbp2014-2024.rds")

## Decision tree for downs 1-3

## Red zone vs other part of field

## Run vs pass

# Run - fumble (fumble_lost 148, fumble_forced 141, fumble 164) vs not (fumble_not_forced 142)
# rush_attempt - col 152
# Pass - incomplete (col 128), complete (complete_pass 165), interception (col 130)
# pass_attempt - col 151

# For complete passes and run, use inferred models 


library(dplyr)
library(stringr)

### Summary for passing plays 

play_summary <- football_data2014 %>%
  filter(play_type %in% c("run", "pass")) %>%
  mutate(
    yrdln_clean = as.numeric(str_extract(yrdln, "\\d+")),  # Extract yard number
    red_zone = ifelse(str_detect(yrdln, defteam) & yrdln_clean <= 20, "Red Zone", "Non-Red Zone")
  ) %>%
  group_by(red_zone, play_type) %>%
  summarise(
    total_plays = n(),
    avg_yards = mean(yards_gained, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(red_zone) %>%
  mutate(percentage = total_plays / sum(total_plays) * 100)  # Compute percentage

# Display the summary
print(play_summary)


### Summary for passing plays  

pass_summary <- football_data2014 %>%
  filter(play_type == "pass") %>%
  mutate(
    yrdln_clean = as.numeric(str_extract(yrdln, "\\d+")),
    red_zone = ifelse(str_detect(yrdln, defteam) & yrdln_clean <= 20, "Red Zone", "Non-Red Zone")
  ) %>%
  group_by(red_zone) %>%
  summarise(
    total_passes = n(),
    completions = sum(complete_pass, na.rm = TRUE),
    incompletions = sum(incomplete_pass, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    avg_yards = mean(yards_gained, na.rm = TRUE)
  ) %>%
  mutate(
    completion_pct = completions / total_passes * 100,
    incompletion_pct = incompletions / total_passes * 100,
    interception_pct = interceptions / total_passes * 100
  )

# Display results
print(pass_summary)

###

### Summary for run plays

run_summary <- football_data2014 %>%
  filter(play_type == "run") %>%
  mutate(
    yrdln_clean = as.numeric(str_extract(yrdln, "\\d+")),
    red_zone = ifelse(str_detect(yrdln, defteam) & yrdln_clean <= 20, "Red Zone", "Non-Red Zone")
  ) %>%
  group_by(red_zone) %>%
  summarise(
    total_runs = n(),
    fumbles = sum(fumble_lost, na.rm = TRUE),
    avg_yards = mean(yards_gained, na.rm = TRUE)
  ) %>%
  mutate(
    fumble_pct = fumbles / total_runs * 100
  )

# Display results
print(run_summary)

### Total Summary - Starting with Field position, then Play type (pass/run), 
### then outcomes (complete, incomplete, interception, fumble)

combined_summary <- football_data2014 %>%
  mutate(
    yrdln_clean = as.numeric(str_extract(yrdln, "\\d+")),
    red_zone = ifelse(str_detect(yrdln, defteam) & yrdln_clean <= 20, "Red Zone", "Non-Red Zone")
  ) %>%
  group_by(red_zone, play_type) %>%
  summarise(
    total_plays = n(),
    avg_yards = mean(yards_gained, na.rm = TRUE),
    
    # For passes: Incomplete, Complete, Interception
    incompletions = sum(incomplete == 1, na.rm = TRUE),
    completions = sum(complete == 1, na.rm = TRUE),
    interceptions = sum(interception == 1, na.rm = TRUE),
    
    # For runs: Fumbles
    fumbles = ifelse(play_type == "run", sum(fumble, na.rm = TRUE), NA)
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
print(combined_summary)
