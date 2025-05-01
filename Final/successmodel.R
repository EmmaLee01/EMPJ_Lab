football_plays$success <- ifelse(football_plays$yards_gained >= football_plays$ydstogo, 1, 0)
success_model <- glm(success ~ ydstogo + yardline_100, data = football_plays, family = binomial())
predict(success_model, newdata = data.frame(ydstogo = 2, yardline_100 = 50), type = "response")






