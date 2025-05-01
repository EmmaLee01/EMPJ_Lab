## Lab 4

# Run play file 


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

down_one <- function(down, ytg, fp) {
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  mu <- 1.2
  sigma <- 1.5 
  ## mu and sigma to create distribution based on what happens on down 1 from dataset
  n <- 1
  gain <- round(sample_yards(1, mix_model)) #  Random gain from log sampling distribution
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)  # Adjust remaining yards for first down 

  if (new_ytg <= 0) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0) # 
  }
  else
    list(down = 2, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}

down_two <- function(down, ytg, fp) {
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  mu <- 1.2
  sigma <- 1.5 # create distribution based on what happens on down 2 from data 
  n <- 1
  gain <- round(sample_yards(1, mix_model)) #  Random gain from sampling distribution
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)  # Adjust remaining yards for first down 
  if (new_ytg <= 0) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  }
  else 
    list(down = 3, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}

down_three <- function(down, ytg, fp) {
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  mu <- 1.2
  sigma <- 1.5 # create distribution based on what happens on down 3 from data
  n <- 1
  gain <- round(sample_yards(1, mix_model)) #  Random gain from sampling distribution
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)  # Adjust remaining yards for first down 
  if (new_ytg <= 0) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0)
  }
  else 
    list(down = 4, ytg = new_ytg, fp = new_fp, exit_drive = 0)


field_goal_probability <- function(FP) {
  
  if (FP >= 95) {
    return(0.99) 
  } else if (FP >= 85) {
    return(0.95)
  } else if (FP >= 75) {
    return(0.85)
  } else if (FP >= 65) {
    return(0.65)
  } else if (FP >= 55) {
    return(0.40)
  } else {
    return(0.10)
  }
}



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
    YG <- round(sample_yards(1, mix_model))  # Simulated yards gained
    YG <- max(YG, 0)  # no negative yards
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
}
