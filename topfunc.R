top_func <- function(downs, ytg, fp, epochs=1000) { ##top function is basically supposed to run both epoch function and the drive, prof. rodu asked to make this a long number
  scores <- epochs ##placeholder for counting scores with the 
  for(i in 1:epochs) {
    state <- list(down = 1, ytg = 10, fp = sample(1:100, 1))
    scores[i] <- run_epoch(downs, ytg, fp)
    
  }
  mean(scores) ##calculate score, expected points
}
