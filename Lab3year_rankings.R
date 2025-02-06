####Question we will be asking: "Now that we have created a steady-state vector for each year of this data, how has LAN's performance changed over the years?"

library(tidyverse)

game_data <- readRDS("data.rds")

# extract all teams
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))
teamlen <- length(teams)

# list to store rankings for each year
yearly_rankings <- list()

# Loop through each season
for (yr in unique(game_data$season)) {
  
  # Filter data for the current year
  yearly_data <- filter(game_data, season == yr)
  
  # transition matrix for current year
  transition_matrix_year <- matrix(0, nrow = teamlen, ncol = teamlen, dimnames = list(teams, teams))
  
  for (i in 1:nrow(yearly_data)) {
    if (yearly_data$Visiting_Score[i] > yearly_data$Home_Score[i]) {
      loser <- yearly_data$Home_Team[i]
      winner <- yearly_data$Visiting_Team[i]
    } else {
      loser <- yearly_data$Visiting_Team[i]
      winner <- yearly_data$Home_Team[i]
    }
    transition_matrix_year[loser, winner] <- transition_matrix_year[loser, winner] + 1
  }
  
  # Each row divided by its row sum to convert counts into probabilities
  transition_matrix_year <- sweep(transition_matrix_year, 1, rowSums(transition_matrix_year), FUN = "/")
  transition_matrix_year[is.na(transition_matrix_year)] <- 0  # Handle empty rows
  
  # PageRank
  ranks <- rep(1/teamlen, teamlen)
  iterations <- 10000
  
  for (i in 1:iterations) {
    ranks <- ranks %*% transition_matrix_year
  }
  
  # Store rankings for the year
  yearly_rankings[[as.character(yr)]] <- sort(setNames(ranks, teams), decreasing = TRUE)
}




cat("\nYear: 2020\n")
print(yearly_rankings[["2020"]])

cat("\nYear: 2021\n")
print(yearly_rankings[["2021"]])

cat("\nYear: 2022\n")
print(yearly_rankings[["2022"]])

cat("\nYear: 2023\n")
print(yearly_rankings[["2023"]])



##Looking at the generated graph of the Los Angeles Dodgers' performance over the years, we can see that their rankings have generally increased with a slight dip
##in 2021. The generally indicates improved performance, though possible explanations could be roster/administration changes, better win percentages, or domantion
##over other teams overall. 
