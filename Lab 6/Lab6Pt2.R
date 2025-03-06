##pt2 PLEASE NOTE THIS ALSO INCLUDES PART 3
nhldata<-read.csv("nhl_pbp20162017.csv")

nhlsub<-nhldata[c(1, 5, 6, 7, 8, 11, 12, 54, 55)]



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


##pt3
## Looking at goal occurrences based on region

nhlsub$ifGoal <- ifelse(nhlsub$Event=="GOAL", 1, 0)


#nhlsub2<-nhldata[nhldata$Event=="GOAL",]
#nhlsub2<-nhlsub2[c(1, 5, 6, 7, 8, 11, 12, 54, 55)]

new_shot_model <- glm(ifGoal ~ region, data = nhlsub, family = poisson())



nhlsub2<-nhlsub[nhlsub$Type %in% shot_types,]
nsm<-glm(ifGoal~factor(Type), data = nhlsub2, family = binomial())










