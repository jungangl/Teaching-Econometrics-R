## This R script completes the programming part of PS7 Q5
## Created by Alex Li



## The following line of code only needs to be run once
## install.packages(c("utils", "dplyr"))
## Import the packages used in this assignment
library(utils)
library(dplyr)



## The IV regression requires the package AER
install.packages("AER")
library(AER)


## save data from file "JEC.csv" in variable jec.data
jec.data <- read.csv("JEC.csv")



## Check the colomn names
names(jec.data)



## Check the first 5 rows of jec.data
head(jec.data, 5)



## Display the summary of jec.data
summary(jec.data)



## Prepare for the regression
jec.data <- mutate(jec.data, log_price = log(price))
jec.data <- mutate(jec.data, log_quantity = log(quantity))



## (a)
## Estimate the demand curve using ordinary least squares.
## The estimate for the coefficient is -0.638884734
## The standard error is 0.08238858
allseasons <- paste("seas", seq(1:12), sep = "")
fmla <- as.formula(paste("log_quantity ~ log_price + ice + ", paste(allseasons, collapse= "+")))
model_a <- lm(data = jec.data, fmla)
summary(model_a)$coefficients[1:2,]



## (c)
## Note that we can have more than one instrument, and we can also include exogenous control variables. Let Y be the outcome (dependent) variable of interest, X be the endogenous variable, W be any exogenous regressors, not including instruments, and Z be the instruments. Then the syntax for ivreg is:
## ivreg(Y ~ X + W | W + Z, ... )
fmla_iv <- as.formula(paste("log_quantity ~ log_price + ice + ", 
                         paste(allseasons, collapse= "+"), 
                         "|", 
                         "ice + ",
                         paste(allseasons, collapse= "+"),
                         "+ cartel")
                   )

iv <- ivreg(data = jec.data, fmla_iv)
summary(iv)$coefficients[1:2,]
## The point estimate is ìmore negativeî than the LS estimate, 
## consistent with LS having a positive bias.



## (d) 
## Re-estimate (c) using Newey-West heteroscedasticity and autocorrelation (HAC) 
## consistent standard errors,
?NeweyWest
nw <- NeweyWest(iv, lag = 4, prewhite = F, adjust = F)
coeftest(iv, vcov = nw)[1:2,]



## The 95% confidence interval for log_price is 
(low <- -0.8665866 - 1.96*0.2224161)
(high <- -0.8665866 + 1.96*0.2224161)
