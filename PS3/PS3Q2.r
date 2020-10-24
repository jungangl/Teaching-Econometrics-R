## This R script completes the programming part of PS3 Q2
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


## (i)
## reestimate the equation from PS2 question 5 in levels form
## regress gexp on pg and inc, including an intercept
model <- lm(data = auto.data, gexp ~ pg + inc)



## (ii)
## construct a plot of the regression residuals against time
## First, save the residuals
reg_resid <- resid(model)



## To complete the homework, it is enough to just plot the residual agains auto.data$t
## However, you will notice that the plot is not that pretty
## Because the class of auto.data$t is factor 
plot(x = auto.data$t, y = reg_resid, xlab = "Time", ylab = "Residuals", pch = 19)



## Now I demonstrate how to convert this factor object to a datetime object
## Note that the class for t is factor
## We want to convert factor to a Date object
class(auto.data$t)
N <- length(auto.data$t)
t_str <- as.character(auto.data$t)



## Create an empty box to hold the vector
date_vec <- vector()
for (i in 1:N){
  str <- ""
  temp <- strsplit(t_str[i], "q")[[1]]
  year <- temp[1]
  mon <- 1 + ((as.integer(temp[2])) - 1) * 3
  day <- "01"
  ## if month < october we need add an extra "0" for the format
  if (mon < 10){
    date_vec <- c(date_vec, paste(year, "0", mon ,day, sep = ""))
  }else{
    date_vec <- c(date_vec, paste(year, mon ,day, sep = ""))
  }
}



## Now we can use lubridate to create the date-time object
install.packages("lubridate")
library(lubridate)



## Now we can plot the residual against ymd(date_vec)
plot(x = ymd(date_vec), 
     y = reg_resid, 
     xlab = "Time", 
     ylab = "Residuals", 
     main = "Non-dff Regression",
     pch = 19)



## Proceed as described in the problem. 
## The graph of residuals do not look like iid disturbances 
## residuals of the same sign are clustered next to each other over time. 
## (There may also be a overall downward slope over time). 
## Thus it is not plausible that assumption A4 
## (nonautocorrelation) is satisfied for the model is level form. 
## This assumption is more plausible for the data in di§erenced form.



## To compare, we can also plot the residual from the difference regression
auto.data <- mutate(auto.data, dgexp = 400 * (gexp - lag(gexp)))
auto.data <- mutate(auto.data, dpg = 400 * (pg - lag(pg)))
auto.data <- mutate(auto.data, dinc = 400 * (inc - lag(inc)))
model_diff <- lm(data = auto.data[-1 ,], dgexp ~ dpg + dinc)
plot(x = ymd(date_vec[-1]), 
     y = resid(model_diff), 
     xlab = "Time", 
     ylab = "Residuals", 
     main = "Diff Regression",
     pch = 19)