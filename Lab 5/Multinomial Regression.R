fthdpredict<-function(FP, YTG){
  newdata<-data.frame(yardline_100 = FP, ydstogo=FP)
  
  probs<-predict(multi_model, newdata = newdata, type="probs")
probs    
}


down_four <- function(D, YTG, FP) {
  
  play_probs <- fthdpredict(FP, YTG)  

  # Sample a play type
  play_type <- sample(names(play_probs), size = 1, prob = play_probs)
  print(paste("Play selected:", play_type)) 
  
  if (play_type == "go_for_it") {
    YG <- sample(0:YTG, 1)  # Simulated yards gained
    FP <- FP + YG
    new_YTG <- max(YTG - YG, 0)
    
    if (new_YTG == 0) {
      exit_drive <- 0  # First down achieved
    } else {
      exit_drive <- 1  # Turnover on downs
    }
    
    YTG <- new_YTG
    D <- 1  # Reset down after conversion/turnover
    
  } else if (play_type == "punt") {
    punt_distance <- sample(35:50, 1)  # Typical punt range
    FP <- max(FP - punt_distance, 0)
    YTG <- 10
    D <- 1
    exit_drive <- 1  # Change possession
    
  } else if (play_type == "field_goal") {
    FG_made <- rbinom(1, 1, field_goal_probability(FP))  # Simulate FG attempt
    
    if (FG_made == 1) {
      FP <- 115
    } 
    
    YTG <- 10
    D <- 1
    exit_drive <- 1  # End possession after FG attempt
  }
  
  list(D = D, YTG = YTG, FP = FP, exit_drive = exit_drive)
}










footballsub<- footballdata[c(12,19,22,26,29,30)]
footballsub2<-footballsub[footballsub$drive==4,]


footballsub3<-footballsub2[footballsub2$play_type=="pass"|
                             footballsub2$play_type=="run"|
                             footballsub2$play_type=="field_goal"|
                             footballsub2$play_type=="punt",]
footballclean<-na.omit(footballsub3)
footballclean

library(dplyr)

footballclean <- footballclean %>%
  mutate(play_type = case_when(
    play_type %in% c("run", "pass") ~ "go_for_it",
    TRUE ~ play_type  # Keep "punt" and "field_goal" the same
  ))

# Convert play_type to a factor
footballclean$play_type <- factor(footballclean$play_type, 
                                  levels = c("go_for_it", "punt", "field_goal"))

library(nnet)

# Fit multinomial regression model
multi_model <- multinom(play_type ~ yardline_100 + ydstogo, data = footballclean)

summary(multi_model)






set.seed(42)
down_four(D = 4, YTG = 5, FP = 35, model = multi_model)

set.seed(42)
down_four(D = 4, YTG = 10, FP = 20, model = multi_model)

set.seed(42)
down_four(D = 4, YTG = 1, FP = 50, model = multi_model)

set.seed(42)
down_four(D = 4, YTG = 5, FP = 95, model = multi_model)

