library(mixtools)
library(tidyverse)
library(mixtools)

footballdata <- readRDS("C:/Users/cemiw/Downloads/pbp2014-2024.rds")
footballsub<- footballdata[c(12,19,22,26,29,30, 37)]
attach(footballsub)
yardfilter<-subset(footballsub, yards_gained>0)
yards<-yardfilter$yards_gained
  





mix_model <- normalmixEM(yards, k = 2)


summary(mix_model)



#OVERALL YARDGAIN HIST
playhistogram<-ggplot(yardfilter, aes(x = yards_gained)) +
  geom_histogram(binwidth = 2, color = "black", fill = "blue", alpha = 0.5)+
  theme_minimal()

##MIXTURE PLOT
mixplot<-ggplot(yardfilter, aes(x = yards_gained)) +
  stat_function(fun = function(x) mix_model$lambda[1] * dnorm(x, mix_model$mu[1], mix_model$sigma[1]), 
                color = "red") +
  stat_function(fun = function(x) mix_model$lambda[2] * dnorm(x, mix_model$mu[2], mix_model$sigma[2]), 
                color = "green") +
  stat_function()
  theme_minimal()
 












##SAMPLING
sample_yards <- function(n, model) {
  component <- sample(1:length(model$lambda), size = n, replace = TRUE, prob = model$lambda)
  sampled_yards <- rnorm(n, mean = model$mu[component], sd = model$sigma[component])
  sampled_yards
}

new_samples <- sample_yards(100, mix_model)

ggplot(data.frame(yards_gained = new_samples), aes(x = yards_gained)) +
  geom_histogram(binwidth = 5, alpha = 0.6) +
  theme_minimal()




