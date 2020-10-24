## This R script completes the programming part of PS2 Q5
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



## Check the first 3 rows to see if dgexp and dpg are added to the data set
head(auto.data, 3)



## (i)
## Use LS to estimate the model from 1959q2 to 1992q1
## Note that "-1" in "auto.data[-1 ,]" omits the first row of the data set
## We need to omit the first row because it contains "NA"
linear_model <- lm(data = auto.data[-1 ,], dgexp ~ dpg + dinc)
(linear_model_summary <- summary(linear_model))
(linear_model_summary$r.squared)



## b2 = -0.1726, SE(b2) = 0.0347
## b3 = 0.3639, SE(b3) = 0.1582
b2 <- -0.1726
b3 <- 0.3639
SE_b2 <- 0.0347
SE_b3 <- 0.1582



## Obtain the critical point
N <- length(linear_model$residuals)
K <- length(linear_model$coefficients)
t_crit <- qt(0.95, N - K)
lower_b2 <- b2 - t_crit * SE_b2
lower_b3 <- b3 - t_crit * SE_b3
higher_b2 <- b2 + t_crit * SE_b2
higher_b3 <- b3 + t_crit * SE_b3



## (ii)
## Create new columns in the dataset
auto.data <- mutate(auto.data, dpg_lag = lag(dpg))
auto.data <- mutate(auto.data, dinc_lag = lag(dinc))



## Estimate the unrestricted model
## R^2 = 0.2250
unrestricted_model <- lm(data = auto.data, dgexp ~ dpg + dinc + dpg_lag + dinc_lag)
(unrestricted_model_summary <- summary(unrestricted_model))
(rsq_unre <- unrestricted_model_summary$r.squared)



## Estimate the restricted model
## R^2* = 0.2215
restricted_model <- lm(data = auto.data, dgexp ~ dpg + dinc)
(restricted_model_summary <- summary(restricted_model))
(rsq_re <- restricted_model_summary$r.squared)



## Compute the F-statistic
N <- length(unrestricted_model$residuals)
K <- length(unrestricted_model$coefficients)
J <- K - length(restricted_model$coefficients)
F_stat <- ((rsq_unre - rsq_re) /  J) / ((1 - rsq_unre) / (N - K))
F_stat



## Get the critical point
F_crit <- qf(0.95, df1 = J, df2 = N - K)



## Compare F_stat and F_crit
## Since F_stat < F_crit,
## we are unable to reject the null hypothesis that
## the lagged variables have no effect
F_stat > F_crit





