
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
current_team <- sample(teams, size = 1)  # Initialize with a random team


for(i in 1:coinpass){ ##For loop
  prob_dist<-transition_matrix[current_team,] ##assigning probability distribution for new coin holder using transition matrix from other file
  current_team <- sample(1:teamlen, size=1, prob=prob_dist) ##sampling new team from specific team probability distribution
  if (i > ignore) { ##loop for when to start adding possessions
    visits[current_team] <- visits[current_team] + 1
  }
}

##team rankings!
team_rankings<-visits/sum(visits) ##normalizing
sort(team_rankings, decreasing = TRUE) ##putting the teams in descending order

##Steady state
#       HOU        LAN        NYA        TOR        BOS        TBA        CLE        SEA        CHN 
#0.03900000 0.03873684 0.03821053 0.03678947 0.03668421 0.03657895 0.03610526 0.03568421 0.03542105 
#       SFN        SLN        NYN        BAL        ATL        MIL        ANA        ARI        WAS 
#0.03526316 0.03494737 0.03489474 0.03468421 0.03357895 0.03342105 0.03321053 0.03263158 0.03194737 
#       TEX        PHI        MIN        PIT        SDN        OAK        DET        COL        KCA 
#0.03194737 0.03178947 0.03157895 0.03147368 0.03094737 0.03021053 0.02984211 0.02978947 0.02968421 
#       CHA        CIN        MIA 
#0.02936842 0.02778947 0.02778947 
