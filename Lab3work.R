# Lab 3 work

game_data <- readRDS("data.rds")

# Load necessary library
library(dplyr)


# Get unique teams
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
n_teams <- length(teams)

# Create a team index for matrix referencing
team_index <- setNames(1:n_teams, teams)

# Initialize transition matrix
transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams))

# Count losses
for (i in 1:nrow(game_data)) {
  if (game_data$Visiting_Score[i] < game_data$Home_Score[i]) {
    loser <- game_data$Visiting_Team[i]
    winner <- game_data$Home_Team[i]
  } else if (game_data$Home_Score[i] < game_data$Visiting_Score[i]) {
    loser <- game_data$Home_Team[i]
    winner <- game_data$Visiting_Team[i]
  } else {
    next  # Ignore ties
  }
  
  # Increment loss count
  transition_matrix[team_index[loser], team_index[winner]] <- 
    transition_matrix[team_index[loser], team_index[winner]] + 1
}

# Normalize each row to sum to 1 (Markov property)
transition_matrix <- transition_matrix / rowSums(transition_matrix)

# Replace NaN (teams that never lost) with uniform probabilities
transition_matrix[is.nan(transition_matrix)] <- 1 / n_teams

# Print the transition probability matrix
head(transition_matrix)

