
data <- data.frame(wage = rep(0, 1000), educ = rep(0, 1000))
abili <-rnorm(1000, 0, 10)
data$educ[abili > 20] <- round(18 + rnorm(length(data$educ[abili > 20]), 0, 2), digits = 0)
data$educ[10 < abili & abili <= 20] <- round(16 + rnorm(length(data$educ[10 < abili & abili <= 20]), 0, 2), digits = 0)
data$educ[0 < abili & abili <= 10] <- round(14 + rnorm(length(data$educ[0 < abili & abili <= 10]), 0, 2), digits = 0)
data$educ[-10 < abili & abili <= 0] <- round(12 + rnorm(length(data$educ[-10 < abili & abili <= 0]), 0, 2), digits = 0)
data$educ[abili <= -10] <- round(10 + rnorm(length(data$educ[abili <= -10]), 0, 2), digits = 0)
data
summary(data)

library(ggplot2)
ggplot(data = data, aes(x = abili)) + geom_density()


data$abili <- abili
data$age <- rnorm(1000, 30, 5)

data$female <- 1 * (order(data$id) > 456)
data
data$id <- NULL



data$iq <- abili + rnorm(1000, 100)
data$wage <- 600 + 12 * data$educ + 24 * data$abili + rnorm(1000, 0, 200) + 5 * data$age - 25 * data$female
data$wage[data$wage < 0] <- 0
summary(data)

plot(data$educ, data$wage)

result <- lm(data = data, wage ~ educ)
summary(result)
data
colnames(data)[1] <- "salary"
write.csv(data, file = "wage.csv", row.names = F)

