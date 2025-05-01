football_plays$success <- ifelse(football_plays$yards_gained >= football_plays$ydstogo, 1, 0)
success_model <- glm(success ~ ydstogo + yardline_100, data = football_plays, family = binomial())
predict(success_model, newdata = data.frame(ydstogo = 2, yardline_100 = 50), type = "response")




simulate_ep <- function(play_type, FP, YTG, reps = 100) {
  results <- replicate(reps, {
    state <- list(fp = FP, down = 4, ytg = YTG, event = NA, points = 0)
    result <- simulate_drive_custom(state, forced_play = play_type)
    result$points
  })
  mean(results)
}

ep_go <- simulate_ep("go_for_it", FP = 45, YTG = 4)
ep_punt <- simulate_ep("punt", FP = 45, YTG = 4)
ep_fg <- simulate_ep("field_goal", FP = 45, YTG = 4)
p_success <- estimated_prob_from_data  

# Use EV formula
ev_actual <- p_success * ep_go + (1 - p_success) * ep_fail

#Show plots of EP vs FP & YTG
#Show where risk-adjusted deviations make sense
#





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
