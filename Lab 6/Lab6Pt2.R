nhldata<-read.csv("nhl_pbp20162017.csv")

nhlsub<-nhldata[c(1, 6, 7, 8, 11, 12, 54, 55)]



nhlsub<-nhlsub[!is.na(nhlsub$xC),]
nhlsub$xC<-as.numeric(nhlsub$xC)
nhlsub$yC<-as.numeric(nhlsub$yC)

define_region <- function(x, y) {
  if (x >= 0 & y >= 0) {
    return("Quad 2")
  } else if (x >= 0 & y < 0) {
    return("Quad 4")
  } else if (x < 0 & y >= 0) {
    return("Quad 1")
  } else {
    return("Quad 3")
  }
}



shot_types <- c("SLAP SHOT", "WRIST SHOT", "BACKHAND","SNAP SHOT", "DEFLECTED","TIP-IN","WRAP-AROUND")
nhlsub$Shots <- ifelse(nhlsub$Type %in% shot_types, 1, 0)

nhlsub$region <- mapply(define_region, nhlsub$xC, nhlsub$yC)
shot_model <- glm(Shots ~ region, data = nhlsub, family = poisson())













