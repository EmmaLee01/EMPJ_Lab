# Drive function

run_drive <- function(state) {
  max <- 100 - state$fp
  min <- -state$fp
  yard_change <- sample(min:max, 1)
  state$fp <- state$fp + yard_change
  state$fp <- max(0, min(100, state$fp)) #in bound
  return(state)
}
  
