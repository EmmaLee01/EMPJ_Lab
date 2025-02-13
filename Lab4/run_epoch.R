
run_epoch <- function(state) {
  max_drives <- 10  # Limit number of drives
  score <- NA       # Initialize score
  drives <- 0       # drive counter
  team <- 1         # 1 for our team, -1 for opponent

  while (is.na(score) && drives < max_drives) {
    state <- run_drive(state)  # Simulate drive
    score <- check_score(state$fp)            # Check if score occurred
    if (!is.na(score)) {
      score <- team * score  # score
    }
    team <- -team  # Switch team
    drives <- drives + 1
  }
  
  if (is.na(score)) score <- 0  # Return 0 if no score after max_drives
  return(score)
}
