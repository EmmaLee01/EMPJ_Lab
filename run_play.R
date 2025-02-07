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
