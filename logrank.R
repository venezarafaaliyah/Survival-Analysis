getwd()
setwd("C:/SEM 7/INTRO SURVIVAL ANALYSIS")
leu <- read.csv("leu.csv")
library(survival)

# load data
leu <- read.csv("C:/SEM 7/INTRO SURVIVAL ANALYSIS/leu.csv")
head(leu)
dim(leu)
factor(leu$Treatment)

# define survival time
y <- Surv(leu$Time,leu$Censor)

# KM curves
# Plot survival curves for each treatment group
plot(survfit(y ~ leu$Treatment),
     xlab = "Time",
     ylab = "Survival Probabilities")

plot(survfit(y ~ leu$Treatment), lty = c("solid", "dashed"), xlab = "Time", ylab = "Survival Probabilities")

legend("topright", c("Placebo", "6-MP"), lty = c("solid", "dashed"))

# Log-rank test
survdiff(y ~ leu$Treatment)

y = Surv(leu$Time,leu$Censor)
leuexp = survreg(formula = y~Treatment, data=leu, dist= "exponential")
summary(leuexp)
