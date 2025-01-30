# Drive function

run_drive <- function(state) {
  max_gain <- 120 - state$fp
  min_gain <- -state$fp
  yard_change <- sample(min:max, 1)
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
  
  if (state$down > 4) {
    state$down <- 1  # first down opponent
    state$ytg <- 10
    state$fp <- 100 - state$fp  # Flip field position
  }
  return(state)
}
  
