sub_data <- full_data %>%
  dplyr::select(ID, winner, method, gender, wins_total_diff, losses_total_diff, age_diff, height_diff, weight_diff, reach_diff, SLpM_total_diff, SApM_total_diff, sig_str_acc_total_diff, td_acc_total_diff, str_def_total_diff, td_def_total_diff, sub_avg_diff, td_avg_diff)



method.data <- filter(sub_data, method=="KO/TKO"| method=="Decision - Split" | method == "Decision - Unanimous" | method == "Decision - Majority" | method == "Submission")
train1.met <- (method.data$ID <= round(0.8*nrow(data), 0))
test1.sub <- method.data[!train1.met, ]

gdata <- full_data %>%
  dplyr::select(ID, gender, winner, wins_total_diff, losses_total_diff, age_diff, weight_diff, SLpM_total_diff, SApM_total_diff, sig_str_acc_total_diff, td_def_total_diff, td_avg_diff) %>%
  drop_na()

#women model
women$ID <- 1:nrow(women)
women <- dplyr::filter(gdata, gender == "Women")
women <- women  %>% dplyr::select(ID, winner, wins_total_diff, losses_total_diff, age_diff, weight_diff, SLpM_total_diff, SApM_total_diff, sig_str_acc_total_diff, td_def_total_diff, td_avg_diff)

train1.wom <- (women$ID <= round(0.8*nrow(women), 0))
test1.wom <- women[!train1.wom, ]
winner.test1.wom <- women$winner[!train1.wom]

lda.fit.women <- lda(winner ~ ., data = women , subset=train1.wom)
lda.pred.wom <- predict(lda.fit.women, test1.wom)
lda.class <- lda.pred.wom$class
method.test1 <- women$method[!train1.met]
table(lda.class, winner.test1.wom)





# LDA
lda.fit.final <- lda(winner ~ ., data = data, subset = train1)
lda.pred <- predict(lda.fit, test1)
lda.class <- lda.pred$class
table(lda.class, winner.test1)

fightnight1.data <- data.frame(
  ID = 1, wins_total_diff = -6, losses_total_diff = -5, age_diff = -4, weight_diff = 0, SLpM_total_diff = 0.43, SApM_total_diff = -1.78, sig_str_acc_total_diff = -0.2, td_def_total_diff = -.17, td_avg_diff=0.3
)

fightnight2.data <- data.frame(
  ID = 2, wins_total_diff = 20, losses_total_diff = 15, age_diff = 10, weight_diff = -18, SLpM_total_diff = -5.37, SApM_total_diff = -0.71, sig_str_acc_total_diff = -0.06, td_def_total_diff = 0.5, td_avg_diff=0.44
)

fightnight3.data <- data.frame(
  ID = 3, wins_total_diff = 2, losses_total_diff = 2, age_diff = 6, weight_diff = -0.6, SLpM_total_diff = -1.68, SApM_total_diff = -1.57, sig_str_acc_total_diff = -0.08, td_def_total_diff = 0.18, td_avg_diff=-0.51
)

fightnight4.data <- data.frame(
  ID = 4, wins_total_diff = 4, losses_total_diff = 6, age_diff = -3, weight_diff = -0.6, SLpM_total_diff = 1.81, SApM_total_diff = 0.88, sig_str_acc_total_diff = 0.09, td_def_total_diff = -0.24, td_avg_diff=-1.85
)

fightnight5.data <- data.frame(
  ID = 5, wins_total_diff = -4, losses_total_diff = 1, age_diff = -6, weight_diff = -3.5, SLpM_total_diff = 0.29, SApM_total_diff = -0.28, sig_str_acc_total_diff = 0.04, td_def_total_diff = 0.15, td_avg_diff=-0.2
)

fightnight6.data <- data.frame(
  ID = 6, wins_total_diff = -9, losses_total_diff = -4, age_diff = -3, weight_diff = 4.7, SLpM_total_diff = 5.06, SApM_total_diff = 5.28, sig_str_acc_total_diff = 0.1, td_def_total_diff = -0.35, td_avg_diff=-2.41
)

fightnight7.data <- data.frame(
  ID = 7, wins_total_diff = -1, losses_total_diff = -1, age_diff = 1, weight_diff = 6, SLpM_total_diff = 2.45, SApM_total_diff = -0.79, sig_str_acc_total_diff = -0.11, td_def_total_diff = 0.5, td_avg_diff=-0.99
)

fightnight8.data <- data.frame(
  ID = 8, wins_total_diff = 0, losses_total_diff = 0, age_diff = 4, weight_diff = -0.5, SLpM_total_diff = 4.75, SApM_total_diff = -0.61, sig_str_acc_total_diff = -0.07, td_def_total_diff = -0.26, td_avg_diff=.63
)

fightnight9.data <- data.frame(
  ID = 9, wins_total_diff = 0, losses_total_diff = -2, age_diff = 1, weight_diff = 0, SLpM_total_diff = -.05, SApM_total_diff = -2.58, sig_str_acc_total_diff = 0.09, td_def_total_diff = 0, td_avg_diff=-1.53
)

fightnight10.data <- data.frame(
  ID = 10, wins_total_diff = -1, losses_total_diff = -2, age_diff = 1, weight_diff = 5, SLpM_total_diff = 1.29, SApM_total_diff = -0.47, sig_str_acc_total_diff = 0.14, td_def_total_diff = -0.05, td_avg_diff=-0.18
)

lda.pred.fightnight1 <- predict(lda.fit, fightnight1.data) #garry .63 red 
lda.pred.fightnight2 <- predict(lda.fit, fightnight2.data) #mingyang .81 blue 
lda.pred.fightnight3 <- predict(lda.fit, fightnight3.data) #Onama .56 blue 
lda.pred.fightnight4 <- predict(lda.fit, fightnight4.data)
lda.pred.fightnight5 <- predict(lda.fit, fightnight5.data) #brown .55 red 
lda.pred.fightnight6 <- predict(lda.fit, fightnight6.data)
lda.pred.fightnight7 <- predict(lda.fit, fightnight7.data) #
lda.pred.fightnight8 <- predict(lda.fit, fightnight8.data) #
lda.pred.fightnight9 <- predict(lda.fit, fightnight9.data) #
lda.pred.fightnight10 <- predict(lda.fit, fightnight10.data) #


lda.class.test <- lda.pred.fightnight1$class
lda.class.test

lda.pred.fightnight3


gdata <- full_data %>%
  dplyr::select(ID, winner, gender, wins_total_diff, losses_total_diff, age_diff, weight_diff, SLpM_total_diff, SApM_total_diff, sig_str_acc_total_diff, td_def_total_diff, td_avg_diff)
gdata
women <- dplyr::filter(gdata, gender == "Women")
women
