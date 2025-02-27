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
