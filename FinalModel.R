#loadingdata
full_data <- read.csv("large_dataset.csv")
data <- full_data %>%
  dplyr::select(ID, winner, wins_total_diff, losses_total_diff, age_diff, weight_diff, SLpM_total_diff, SApM_total_diff, sig_str_acc_total_diff, td_def_total_diff, td_avg_diff) %>%
  drop_na()

data$winner = as.factor(data$winner)

# LDA TESTING MODEL
lda.fit <- lda(winner ~ .-ID, data = data, subset = train1)
lda.pred <- predict(lda.fit, test1)
lda.class <- lda.pred$class
table(lda.class, winner.test1)
89+66+288+1003 # number of observations in test set used for analysis

(66+288)/(89+66+288+1003) # overall error rate
66/(89+66) # Blue class error rate
288/(288+1003) # Red class error rate

#Final Model
lda.fit.final <- lda(winner ~ .-ID, data = data)
lda.pred <- predict(lda.fit.final, data)
lda.class <- lda.pred$class
table(lda.class, data$winner)
979/(979+312) # 0.7583269 red accuracy
95/(95+60) #0.6129032 blue accuracy
(979 + 95)/(1446) #0.7427386 overall
