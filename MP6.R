setwd("F:/Academic/Fall 18@UTD/Statistical Methods for DS/Mini Projects")
pro_canc <- read.csv(file = "prostate_cancer.csv", header=TRUE, sep=",")
attach(pro_canc)
str(pro_canc)
boxplot(pro_canc$psa)
boxplot(log(pro_canc$psa, base = exp(1)))

table(pro_canc$subject)
table(pro_canc$cancervol)
table(pro_canc$weight)
table(pro_canc$age)
table(pro_canc$benpros)
table(pro_canc$vesinv)
table(pro_canc$capspen)
table(pro_canc$gleason)
#Take log(psa) as response
y <- log(pro_canc$psa, base = exp(1))

plot(pro_canc$cancervol, y)
fit1 <- lm(y ~ cancervol, data = pro_canc)
abline(fit1)

plot(pro_canc$weight, y)
fit2 <- lm(y ~ weight, data = pro_canc)
abline(fit2)

plot(pro_canc$age, y)
fit3 <- lm(y ~ age, data = pro_canc)
abline(fit3)

plot(pro_canc$benpros, y)
fit4 <- lm(y ~ benpros, data = pro_canc)
abline(fit4)

plot(pro_canc$vesinv, y)
fit5 <- lm(y ~ vesinv, data = pro_canc)
abline(fit5)

plot(pro_canc$capspen, y)
fit6 <- lm(y ~ capspen, data = pro_canc)
abline(fit6)

plot(pro_canc$gleason, y)
fit7 <- lm(y ~ gleason, data = pro_canc)
abline(fit7)
