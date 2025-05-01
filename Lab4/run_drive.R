# Drive function

run_drive <- function(state) {
  max_gain <- 120 - state$fp
  min_gain <- -state$fp
  yard_change <- sample(min_gain:max_gain, 1)
  state$fp <- state$fp + yard_change
  state$fp <- max(0, min(120, state$fp)) #in bound
  return(state)

  state$ytg <- max(0, state$ytg - yard_change) # ytg update
  
  if (state$ytg <= 0) {
    state$down <- 1   # back to first down
    state$ytg <- 10
  } else {
    state$down <- state$down + 1  # Increment down
  }
  
  # If 4th down, use model
  if (state$down > 4) {
    result <- handle_fourth_down(down = state$down, ytg = state$ytg, fp = state$fp, multi_model = your_model)
    
    if (result$event == "FIRST_DOWN") {
      state$fp <- state$fp + result$yards
      state$ytg <- 10
      state$down <- 1
      state$event <- "PLAY"
    } else if (result$event == "TURNOVER_ON_DOWNS") {
      state$event <- "TURNOVER_ON_DOWNS"
    } else if (result$event == "FIELD_GOAL_ATTEMPT") {
      state$event <- "FIELD_GOAL_ATTEMPT"
      state$points <- result$points
    } else if (result$event == "PUNT") {
      state$event <- "PUNT"
      state$fp <- result$new_fp
    }
    
    return(state)
  }
  }

