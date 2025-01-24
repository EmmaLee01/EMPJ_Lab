top_func <- function(downs, ytg, fp, epochs=1000) {
  scores <- epochs
  for(i in 1:epochs) {
    scores[i] <- simulate_epoch(downs, ytg, fp)
    
  }
  mean(scores)
}