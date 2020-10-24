## This R script completes the programming part of PS2 Q6
## Created by Alex Li



## The following line of code only needs to be run once
## install.packages(c("utils", "dplyr"))



## Import the packages used in this assignment
library(utils)
library(dplyr)
## save data from file "Growth.csv" in variable auto.data
growth.data <- read.csv("Growth.csv")



## Check the colomn names
names(growth.data)



## Check the first 5 rows of growth.data
head(growth.data, 5)



## Display the summary of growth.data
summary(growth.data)



## Remove the outlier "Malta"
growth.data <- filter(growth.data, country_name != "Malta")



## Run a regression of Growth on TradeShare, YearsSchool, Rev_Coups, Assassinations and GRDP60.
model <- lm(data = growth.data, growth ~ tradeshare + yearsschool + 
                                         rev_coups + assasinations + rgdp60)
(model_summary <- summary(model))
(model_summary$r.squared)



## (a)
## The value of the coefficient on Rev_Coups is -2.1504255
## An additional coup in a Öve year period, reduces the average year growth rate by
## (2.15=5) = 0:43% over this 25 year period.
## This means the GDP in 1995 is expected to be approximately 0:43 * 25 = 10:75% lower.
## This is a large e§ect.
model_summary$coefficients[4, ]



## (b)
## Use the regression to predict the annual average growth rate for a
## country that has average values for all regressors.
## The mean values for all regressors are 
avg <- c(
  1,
  mean(growth.data$tradeshare),
  mean(growth.data$yearsschool), 
  mean(growth.data$rev_coups), 
  mean(growth.data$assasinations), 
  mean(growth.data$rgdp60)
)



## coef(model) returns a named list
## In order to make some mathematical operations 
## You need to first unname the list
coeffs <- unname(coef(model))



## Compute the dot product of mean regressors and estimates of the model
## The predicted growth rate at the mean values for all regressors is 1.87.
(avg %*% coeffs)



## (c)
## Now assume that the countryís value for 
## TradeShare is one standard deviation above the mean.
## The SD of trade share is 0.229. 
avg2 <- c(
  1,
  mean(growth.data$tradeshare) + sd(growth.data$tradeshare),
  mean(growth.data$yearsschool), 
  mean(growth.data$rev_coups), 
  mean(growth.data$assasinations), 
  mean(growth.data$rgdp60)
)



## Compute the dot product of mean regressors and estimates of the model 
## The resulting predicted value is 2.18.
(avg2 %*% coeffs)



## (d)
## Test whether, taken as a group, YearsSchool, Rev_Coups, Assas-inations 
## and GRDP60, can be omitted from the regression.
## Unrestricted R-squared = 0.2911211
(rsq_unre <- summary(model)$r.squared)



## Restricted R-squared 
## R^2* = 0.04465809
restricted_model <- lm(data = growth.data, growth ~ tradeshare)
(restricted_model_summary <- summary(restricted_model))
(rsq_re <- restricted_model_summary$r.squared)



## Compute the F-statistic
N <- length(model$residuals)
K <- length(model$coefficients)
J <- K - length(restricted_model$coefficients)
F_stat <- ((rsq_unre - rsq_re) /  J) / ((1 - rsq_unre) / (N - K))
(p_value <- pf(F_stat, J, N - K, lower.tail = F))



## Or you can use the canned package in R to do this F test
## This method is contained in package "car"
install.packages("car")
library(car)
linearHypothesis(model,c("yearsschool = 0", 
                         "rev_coups = 0", 
                         "assasinations = 0", 
                         "rgdp60"), test="F")



