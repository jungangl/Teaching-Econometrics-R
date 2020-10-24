## This R script completes the programming part of PS4 Q1
## Created by Alex Li



## The following line of code only needs to be run once
## install.packages(c("utils", "dplyr"))



## Import the packages used in this assignment
library(utils)
library(dplyr)



## save data from file "AUTO1.csv" in variable auto.data
auto.data <- read.csv("AUTO1.csv")



## Check the colomn names
names(auto.data)



## Check the first 5 rows of auto.data
head(auto.data, 5)



## Display the summary of auto.data
summary(auto.data)



## Define the following new variables:
## dgexp = 400  (gexp - gexp(-1)) 
## dpg = 400  (pg - pg(-1))
## dinc = 400  (inc - inc(-1))
auto.data <- mutate(auto.data, dgexp = 400 * (gexp - lag(gexp)))
auto.data <- mutate(auto.data, dpg = 400 * (pg - lag(pg)))
auto.data <- mutate(auto.data, dinc = 400 * (inc - lag(inc)))



## (a)
## Start with regressing dgexp on dpg and dinc
## over the whole period that you estimated in PS2
## i.e. 1959Q2 to1992Q1.
model_pooled <- lm(data = auto.data[-1 ,], dgexp ~ dpg + dinc)
(model_pooled_summary <- summary(model_pooled))



## For the pooled regression we have SSE = 5750.856
(SSE_pooled <- sum(resid(model_pooled) ^ 2))



## Subperiod 1
## Note 1959Q2 and 1975Q4 correspond to row 2 and row 68
## Regression 1 is as follows
model1 <- lm(data = auto.data[2:68 ,], dgexp ~ dpg + dinc)
# SSE1 = 2328.447
(SSE1 <- sum(resid(model1) ^ 2))



## Subperiod 2
## Note 1976Q1 and 1992Q1 correspond to row 69 and row 133
## Regression 2 is as follows
model2 <- lm(data = auto.data[69:133 ,], dgexp ~ dpg + dinc)
# SSE2 = 2670.055
(SSE2 <- sum(resid(model2) ^ 2))



## Compute the F-statistic
## Use the formula: 
## F = ((SSE* - (SSE1 + SSE2))/K)/((SSE1 + SSE2)/(n1 + n2 - 2K))
K <- length(model_pooled$coefficients)
n1 <- nrow(auto.data[2:68 ,])
n2 <- nrow(auto.data[69:133 ,])
F_stat <- ((SSE_pooled - (SSE1 + SSE2))/K)/((SSE1 + SSE2)/(n1 + n2 - 2*K))
## Since F:05(3;126)  2:65 we easily reject the null hypothesis 
## of parameter constancy at the 5% level.



## (b)
## The largest residual is in 1974q1 (a negative residual of -30.78).
## The following method shows that the index of the largest resudual in absolute value in auto.data is 61
##In the model the index is 60 because we left out the first row of the data
which.max(abs(resid(model_pooled)))
auto.data[61, ]
resid(model_pooled)[60]



##(c) 
## Construct the following two variables: D74Q1=1 in 1974Q1 and 0 otherwise
auto.data$d74q1 = as.integer(as.character(auto.data$t) == "1974q1")



## Construct D74perm = 1 on or after 1974Q1 and 0 before 1974Q1.
auto.data$index = 1:nrow(auto.data)
auto.data <- mutate(auto.data, d74perm  = as.integer(index >= 61))



## Estimate the regression
auto.data <- mutate(auto.data, ld74q1 = lag(d74q1))
model_c <- lm(data = auto.data, dgexp ~ dpg + dinc + d74q1 + ld74q1 + d74perm)
(model_c_summary <- summary(model_c))



## Compare the estimates for beta2 and beta3
model_pooled_summary$coefficients
model_c_summary$coefficients



## (d)
## To allow for the two slope parameters beta1 and beta2
## to change beginning 1974Q1, add to the regression the two variables 
## the two variables dpgd74perm and dincd74perm.
model_d <- lm(data = auto.data, dgexp ~ dpg + dinc + d74q1 + ld74q1 + d74perm + dpg:d74perm + dinc:d74perm)
(model_d_summary <- summary(model_d))



## Then use an F-test based on the SSE of this (unrestricted)
SSEd_restricted <- sum(resid(model_c) ^ 2)
SSEd_unrestricted <- sum(resid(model_d) ^ 2)
K_d <- length(model_d$coefficients)
J_d <- K_d - length(model_c$coefficients)
N_d <- length(resid(model_d))
(F_stat_d <- ((SSEd_restricted - SSEd_unrestricted) / J_d)/(SSEd_unrestricted / (N_d - K_d)))



## Since F_0.05(2,124) = 3.05, we are unable to reject the null hypothesis that beta2 and beta3 
## are the same before and after 1974Q1.
qf(0.95, J_d, N_d - K_d)