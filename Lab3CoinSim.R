
##Reading in Data
game_data <- readRDS("data.rds") 

##Creating a vector for all teams
teams<-unique(c(game_data$Visiting_Team, game_data$Home_Team)) 
teamlen<-length(teams)

##Coin pass simulation
team_index <- setNames(1:teamlen, teams) ##indexing teams
coinpass<-20000 ##Number of passes
ignore<-1000 ##number of ignores until coin counts are actually counted
visits<-rep(0, teamlen) ##coin count vector
names(visits)<-teams ## labeling

for(i in 1:coinpass){
  prob_dist<-transition_matrix[current_team,]
  current_team <- sample(1:teamlen, size=1, prob=prob_dist)
  if (i > ignore) {
    visits[current_team] <- visits[current_team] + 1
  }
}

##team rankings!
team_rankings<-visits/sum(visits)
sort(team_rankings, decreasing = TRUE)
