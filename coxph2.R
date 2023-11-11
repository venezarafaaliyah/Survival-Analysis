###################################################################
#
# @filename : coxph2.R
# @brief    : R code for topic 5: the Cox PH models
# @date     : 20230426
# 
###################################################################

getwd()
#setwd()

#A function for -log-log
minusloglog <- function(p){
  return(-log(-log(p)))}


## Installing required packages
#install.packages(c("survival"))
library(survival)

# Load leukemia data
leu <- read.csv("C:/SEM 7/INTRO SURVIVAL ANALYSIS/leu3.csv")
head(leu)
dim(leu)
factor(leu$Treatment)

# Defining survival outcome
y <- Surv(leu$Time,leu$Censor)


# Model 2: h(t|X) = h_0(t) x exp(b1 x Treatment + b2 x logWBC)
model2 <- coxph(y ~ leu$Treatment + leu$logWBC)
summary(model2)


# log-log plot 

## Rscript-1
### Treatment
kmfit_leu <- survfit(y ~ leu$Treatment)
plot(kmfit_leu, fun = minusloglog, xlab ="Time", ylab = "-log-log S", lty = c("solid", "dashed"))
legend("topright", c("Placebo", "6-MP treatment"), lty = c("solid", "dashed"))

## Rscript-2
### logWBC
leu$logWBC_cat <- cut(leu$logWBC, breaks = c(0, 2.3, 3, 100), labels = c("Low", "Medium", "High"))
head(leu)
kmfit_leu2 <- survfit(y ~ leu$logWBC_cat)
names(kmfit_leu2)
plot(kmfit_leu2, fun = minusloglog, xlab ="Time", ylab = "-log-log S", col = c("black", "red", "blue"))
legend("topright", c("Low", "Medium", "High"), lty=c("solid"), col = c("black", "red", "blue"))


## Rscript-3
# New model with 3 predictors
# h(t|X) = h_0(t) x exp(b1 x Treatment + b2 x logWBC + b3 x Sex)
model4 <- coxph(y ~ Treatment + logWBC + Sex, data = leu)
summary(model4)

# Testing the correlations on Schoenfeld resids and their ranks
?cox.zph
check_ph <- cox.zph(model4, transform = rank)
check_ph$y
check_ph$table

# KM curves
plot(survfit(y ~ leu$Treatment), lty = c("solid", "dashed"), xlab = "Time", ylab = "Survival Probabilities")
legend("topright", c("Placebo", "6-MP"), lty = c("solid", "dashed"))

plot(survfit(y ~ leu$logWBC_cat), xlab = "Time", ylab = "Survival Probabilities", col = c("black", "red", "blue"))
legend("topright", c("Low", "Medium", "High"), lty=c("solid"), col = c("black", "red", "blue"))

plot(survfit(y ~ leu$Sex), lty = c("solid", "dashed"), xlab = "Time", ylab = "Survival Probabilities")
legend("topright", c("Female", "Male"), lty=c("solid", "dashed"))


leu$logWBC_cat <- cut(leu$logWBC, breaks = c(0, 
                                             2.3, 3, 100), labels = c("Low", "Medium", 
                                                                      "High"))
kmfit_leu2 <- survfit(y ~ leu$logWBC_cat)
names(kmfit_leu2)
plot(kmfit_leu2, fun = minusloglog, xlab ="Time", 
       ylab = "-log-log S", col = c("black", 
                                    "red", "blue"))
legend("topright", c("Low", "Medium", "High"), 
         lty=c("solid"), col = c("black", "red", 
                                 "blue"))
kmfit_leu <- survfit(y ~ leu$Treatment)
plot(kmfit_leu, fun = minusloglog, xlab ="Time", 
       ylab = "-log-log S", lty = c("solid", 
                                    "dashed"))
legend("topright", c("Placebo", "6-MP 
treatment"), lty = c("solid", "dashed"))