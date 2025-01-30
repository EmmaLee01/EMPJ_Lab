# Drive function

run_drive <- function(state) {
  max <- 100 - state$fp
  min <- -state$fp
  yard_change <- sample(min:max, 1)
  state$fp <- state$fp + yard_change
  state$fp <- max(0, min(100, state$fp)) #in bound
  return(state)

  if (state$fp >= 100) {
    state$score <- ifelse(state$fp >= 110, 3, 7)
    } else {
    state$score <- 0
  }
}
  
