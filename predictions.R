

# LDA
lda.fit.final <- lda(winner ~ ., data = data)
lda.pred <- predict(lda.fit, test1)
lda.class <- lda.pred$class
table(lda.class, winner.test1)

fightnight1.data <- data.frame(
  ID = 1, wins_total_diff = -6, losses_total_diff = -5, age_diff = -4, weight_diff = 0, SLpM_total_diff = 0.43, SApM_total_diff = -1.78, sig_str_acc_total_diff = -0.2, td_def_total_diff = -.17, td_avg_diff=0.3
)

fightnight2.data <- data.frame(
  ID = 1, wins_total_diff = 20, losses_total_diff = 15, !!age_diff = -4, weight_diff = 0, SLpM_total_diff = 0.43, SApM_total_diff = -1.78, sig_str_acc_total_diff = -0.2, td_def_total_diff = -.17, td_avg_diff=0.3
)

lda.pred.fightnight1 <- predict(lda.fit.final, fightnight1.data)
lda.class.test <- lda.pred.fightnight1$class
lda.class.test

lda.pred.fightnight1


gdata <- full_data %>%
  dplyr::select(ID, winner, gender, wins_total_diff, losses_total_diff, age_diff, weight_diff, SLpM_total_diff, SApM_total_diff, sig_str_acc_total_diff, td_def_total_diff, td_avg_diff)
gdata
women <- dplyr::filter(gdata, gender == "Women")
women
