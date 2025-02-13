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

down_one <- function(down, ytg, fp){
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  gain <- sample(0:10, 1)  #  Random gain between 0 and 10 yards
  new_fp <- fp + gain
  new_ytg <- max(10 - gain, 0)  # Adjust remaining yards for first down
  list(fp = new_fp, ytg = new_ytg, down = 2)
  if (new_ytg <= 0) {
    return(list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0))
  }
  return(list(down = 2, ytg = new_ytg, fp = new_fp, exit_drive = 0))
}


down_one <- function(down, ytg, fp) {
  # Simulate play outcome (e.g., gain yards, turnover, etc.)
  mu <- 1.2
  sigma <- 1.5 
  ## mu and sigma to create distribution based on what happens on down 1 from dataset
  n <- 1
  gain <- rlnorm(n, mu, sigma) #  Random gain from log sampling distribution
  new_fp <- fp + gain
  new_ytg <- max(ytg - gain, 0)  # Adjust remaining yards for first down 

  if (new_ytg <= 0) {
    list(down = 1, ytg = 10, fp = new_fp, exit_drive = 0) # 
  }
  else
    list(down = 2, ytg = new_ytg, fp = new_fp, exit_drive = 0)
}


down_four <- function(D, YTG, FP) {  
  
  if (FP <= 40) {  
    play_probs <- c("field_goal" = 0.648, "pass" = 0.151, "run" = 0.090, "punt" = 0.048, "no_play" = 0.057, "quarter_end" = 0.004, "qb_kneel" = 0.0013)  
  } else if (FP <= 60) {  
    play_probs <- c("field_goal" = 0.005, "pass" = 0.115, "run" = 0.063, "punt" = 0.747,   
                    "no_play" = 0.067, "quarter_end" = 0.003, "qb_kneel" = 0.0004)  
  } else if (FP <= 80) {  
    play_probs <- c("pass" = 0.045, "run" = 0.028, "punt" = 0.889, "no_play" = 0.039,   
                    "quarter_end" = 0.0004, "qb_kneel" = 0.0004)  
  } else {  
    play_probs <- c("pass" = 0.027, "run" = 0.009, "punt" = 0.925, "no_play" = 0.039,   
                    "quarter_end" = 0.0005, "qb_kneel" = 0.0005)  
  }  
  
  # Sample a play type  
  play_type <- sample(names(play_probs), size = 1, prob = play_probs)  
  
  if (play_type %in% c("pass", "run")) {  
    YG <- sample_yards(D, YTG, FP)  # Sample yards gained  
    FP <- FP + YG  
    new_YTG <- max(YTG - YG, 0)  
    
    if (new_YTG == 0) {  
      exit_drive <- 0  # Continue drive (first down achieved)  
    } else {  
      exit_drive <- 1  # Turnover 4th down (possession changes)  
    }  
    
    YTG <- new_YTG  
  } else if (play_type == "punt") {  
    FP <- max(FP - sample(35:50, 1), 0)  # Typical punt distance  
    YTG <- 10  
    D <- 1  
    exit_drive <- 1  # Switch possession  
  } else if (play_type == "field_goal") {  
    FG_made <- rbinom(1, 1, field_goal_probability(FP))  # Check if FG is successful  
    
    if (FG_made == 1) {  
      FP <- 115  
    }  
    
    YTG <- 10  
    D <- 1  
    exit_drive <- 1  # End possession  
  } else {  # Covers no play, quarter end, and qb kneel  
    if (play_type == "quarter_end") {  
      exit_drive <- 0  # Continue drive  
    } else {  
      exit_drive <- 1  # End possession  
    }  
  }  
  
  list(D = D, YTG = YTG, FP = FP, exit_drive = exit_drive)  
}
