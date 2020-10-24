## This R script completes the programming part of PS1 Q6
## Created by Alex Li



## The following line of code only needs to be run once
install.packages(c("utils", "dplyr"))



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



## (i)
## Define the following new variables:
## dgexp = 400  (gexp - gexp(-1)) 
## dpg = 400  (pg - pg(-1))
auto.data <- mutate(auto.data, dgexp = 400 * (gexp - lag(gexp)))
auto.data <- mutate(auto.data, dpg = 400 * (pg - lag(pg)))



## Check the first 3 rows to see if dgexp and dpg are added to the data set
head(auto.data, 3)



## Use LS to estimate the model from 1959q2 to 1992q1
## lm omits the rows with NA automatically
linear_model <- lm(data = auto.data, dgexp ~ dpg)



## (ii)
## a = 0.7536, SE(a) = 0.5907
## b = -0.1899 , SE(b) = 0.0344
(linear_model_summary <- summary(linear_model))
a <- 0.7536
SE_a <- 0.5907
b <- -0.1899
SE_b <- 0.0344


## R^2 = 0.1896
(linear_model_summary$r.squared)



## (iii)
## Compute the T-stat for the hypothesis test
t_stat <- abs(a / SE_a)



## Get the critical value for the T test
N <- length(linear_model$residuals)
K <- length(linear_model$coefficients)
t_crit <- qt(0.975, N - K)



## Compare the t_stat with the t_crit
## t_stat < t_crit
## So we are unable SE(a) to reject H0 at the 5% level.
(t_stat > t_crit)



## (iv)
## Compute the 90% confidence interval for beta
t_0.05 <- qt(0.95, N - K)
(lower <- b - t_0.05 * SE_b)
(higher <- b + t_0.05 * SE_b)
