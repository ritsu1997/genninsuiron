set.seed(2)
n <- 897
wealth <- round(rnorm(n, 500, 80))
summary(wealth)
breakfast <- round(0.01 * wealth + rnorm(n, 0, 1.5))
breakfast[breakfast > 7] <- 7
breakfast[breakfast < 0] <- 0
plot(breakfast, score)

absent_days <- round(rnorm(n, 10, 3))
summary(absent_days)
absent_days[absent_days < 0] <- 0

score <- 0.13 * wealth - 2 * absent_class + rnorm(n, 0, 10)
summary(score)
score[score < 0] <- 0
score[score > 100] <- 100

grade <- data.frame(score, wealth, breakfast, absent_days)

result <- lm(data = grade, score ~ breakfast)
summary(result)

result <- lm(data = grade, score ~ breakfast + wealth)
summary(result)

write.csv(grade, file = "breakfast.csv")


plot(breakfast, score)
