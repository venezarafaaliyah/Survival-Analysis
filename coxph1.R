###################################################################
#
# @filename : coxph.R
# @brief    : R code for topic 5: the Cox PH models
# @date     : 20230402
# 
###################################################################

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

# Model 1: h(t|X) = h_0(t) x exp(b1 x Treatment)
model1 <- coxph(y ~ leu$Treatment)
summary(model1)

# Model 2: h(t|X) = h_0(t) x exp(b1 x Treatment + b2 x logWBC)
model2 <- coxph(y ~ leu$Treatment + leu$logWBC)
summary(model2)

# Model 3: h(t|X) = h_0(t) x exp(b1 x Treatment + b2 x logWBC + b3 x Treatment*logWBC)
model3 <- coxph(y ~ leu$Treatment + leu$logWBC + 
                  leu$Treatment*leu$logWBC)
summary(model3)

model3$loglik[2]
model2$loglik[2]
LR=-2*model2$loglik[2] - (-2*model3$loglik[2])
summary(LR)

model1$var
sqrt(model1$var)
model2$var
sqrt(model2$var)
model3$var
sqrt(model3$var)

0.18045368+0.11282719+(-0.01128995)*(0.4^2)
