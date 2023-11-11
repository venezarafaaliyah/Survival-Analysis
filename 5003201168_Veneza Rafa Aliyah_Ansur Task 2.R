###################################################################
#
# @filename : logrank_asg2.R
# @brief    : R code to do assignment 2 for Survival Analysis (Q)
# @student number     : 5003201168
# @author   : Veneza Rafa Aliyah
#
###################################################################

# Klein2003 datasets are pooled in KMsurv package
# Reading materials: https://rdrr.io/cran/KMsurv/

#install.packages("KMsurv") # Installing KMsurv. You need to remove '#' up front to run this line.
library(KMsurv)
## Loading other required packages
library(survival)
library(ggsurvfit)
library(tidycmprsk)


# R code for question (1) and (2)
## Load your data
catether<- read.csv("C:/SEM 7/INTRO SURVIVAL ANALYSIS/catheter.csv",sep=";")
head(catether)
dim(catether)
factor(catether$Treatment)

## Define your survival time
## (complete the following code)
y <- Surv(catether$Time,catether$Censor)

## Plotting KM curves with 95% confidence bands
## (complete the following code)
kmfit_cat <- survfit(y ~ catether$Treatment)
kmfit_cat %>%
  ggsurvfit() +
  labs(
    x = "Time",
    y = "Survival probabilities"
  ) + 
  add_confidence_interval()

## Evaluate the difference between surgical vs percutaneous catheterization
## (complete the following code)
survdiff(y ~ catether$Treatment)


# R code for question (3): time-to-infection on burn patients
## Read more about the data description here: https://rdrr.io/cran/KMsurv/man/burn.html
## Our variable of interest:
### Z1: cleansing method (0=routine bathing 1=body cleansing)
### T3: Time to straphylocous aureaus infection or on study time
### D3: Straphylocous aureaus infection: 1=yes 0=no

data(burn) # loading data called burn
dim(burn) # getting the dimension of burn data (154 rows by 18 columns)
head(burn) 
# define survival time
y = Surv(burn$T3,burn$D3)

# Log-rank test
survdiff(y ~ burn$Z1)

# R code for question (4)
## Load your data
## (write your code here)
boron=read.csv("C:/Users/Asus/Downloads/boron.csv")
## Define your survival time
## (complete the following code)

y <- Surv(boron$Time,boron$Censored)

## Evaluate the difference in survival curves among the three groups
## (complete the following code)
survdiff(y ~ boron$Treatment)

# 95% confidence intervals
kmfit <- survfit(y ~ boron$Treatment)
summary(kmfit) # Get the 95% CIs for curve, for each group
kmfit # Get the 95% CIs for median, for each group

kmfit %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Survival probability"
  ) + 
  add_confidence_interval()

# R code for question (5): kidney transplant
## Read more about the data description here: https://rdrr.io/cran/KMsurv/man/kidtran.html
## Our variable of interest:
### time: Time to death or on-study time
### delta: Death indicator (0=alive, 1=dead)
### gender: 1=male, 2=female
### race: 1=white, 2=black

data(kidtran) # loading the kidney transplatation data
dim(kidtran)
head(kidtran)

# Re-group data based on gender x race combination
# group 1: male-white (gender=1, race=1)
# group 2: male-black (gender=1, race=2)
# group 3: female-white (gender=2, race=1)
# group 4: female-black (gender=2, race=2)
# Create a new column called 'group'
kidtran$group <- ifelse(kidtran$gender==1 & kidtran$race==1, 1,
                        ifelse(kidtran$gender==1 & kidtran$race==2, 2,
                               ifelse(kidtran$gender==2 & kidtran$race==1, 3,4 )))

## You may want to check your updated kidtran dataframe
head(kidtran)

## Define your survival time
## (complete the following code)
y <- Surv(kidtran$time,kidtran$delta)

## Evaluate the difference in survival curves among the three groups
## (complete the following code)
survdiff(y ~ kidtran$group)

## Summary of 95% confidence interval for curves and medians
## (complete the following code)
kmfit_kidtran <- survfit(y ~ kidtran$group)
summary(kmfit_kidtran) # Get the 95% CIs for curve, for each group
kmfit_kidtran # Get the 95% CIs for median, for each group

kmfit_kidtran %>%
  ggsurvfit() +
  labs(
    x = "Time",
    y = "Survival probability"
  ) + 
  add_confidence_interval()
                                   