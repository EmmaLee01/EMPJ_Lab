## Lab 4

# Run play file 

# Different down functions

down_one <- function(fp, ytg) {
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  gain <- sample(0:10, 1)  #  Random gain between 0 and 10 yards
  new_position <- fp + gain
  new_distance <- max(10 - gain, 0)  # Adjust remaining yards for first down
  list(fp = new_position, ytg = new_distance, down = 2)
}
## Possible down function ^




run_play <- function(down, ytg, fp) {
  if (down == 1) {
    return(down_one(down, ytg, fp))
  } else if (down == 2) {
    return(down_two(down, ytg, fp))
  } else if (down == 3) {
    return(down_three(down, ytg, fp))
  } else {
    return(down_four(down, ytg, fp))
  }
}

down_one <- function(down, ytg, fp){
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  gain <- sample(0:10, 1)  #  Random gain between 0 and 10 yards
  new_fp <- fp + gain
  new_ytg <- max(10 - gain, 0)  # Adjust remaining yards for first down
  list(fp = new_position, ytg = new_distance, down = 2)
  if (new_ytg <= 0) {
    return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0))
  }
  return(list(down = 2, ytg = new_ytg, fp = new_fp, exit_drive = 0))
}
