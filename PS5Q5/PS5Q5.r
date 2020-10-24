## This R script completes the programming part of PS5 Q5
## Created by Alex Li



## The following line of code only needs to be run once
## install.packages(c("utils", "dplyr"))
## Import the packages used in this assignment
library(utils)
library(dplyr)



## save data from file "Guns.csv" in variable guns.data
guns.data <- read.csv("Guns.csv")



## Check the colomn names
names(guns.data)



## Check the first 5 rows of guns.data
head(guns.data, 5)



## Display the summary of guns.data
summary(guns.data)



## Note that the class of stateid and year are both integers
class(guns.data$stateid)
class(guns.data$year)



## First demonstrate what the factor method does:
guns.data <- mutate(guns.data, stateid = factor(stateid))
guns.data <- mutate(guns.data, year = factor(year))
class(guns.data$stateid)
class(guns.data$year)



## (a)
## Using pooled least-squares
## (i) a simple regression of ln(vio) on shall,
model_a1 <- lm(data = guns.data, log(vio) ~ shall)
(model_a1_summary <- summary(model_a1))
## The estimated coe¢ cient on shall is -0.443 with a (conventional) SE of 0.042.



## (ii) a multiple regression that adds the controls incarc_rate, density, 
## avginc, pop, pb1064, pw1064, and pm1029.
model_a2 <- lm(data = guns.data, log(vio) ~ shall + incarc_rate + density + avginc + 
                                            pop + pb1064 + pw1064 + pm1029)



## The estimated coefficient is -0.368 with a (conventional) SE of 0.033. 
## The estimated pooled LS coefficient suggests that shall-issue laws 
## reduce violent crime by 36%. This is a large effect.
(model_a2_summary <- summary(model_a2))



## (b) 
## Now do a fixed effect regression, 
## With the same variables as in (ii), 
## but including state fixed effects
## How does this affect the size and significance of the coefficient on shall?
# estimate the fixed effects regression with plm()
model_b <- lm(data = guns.data, 
              log(vio) ~ shall + incarc_rate + density + avginc + pop + 
                         pb1064 + pw1064 + pm1029 + factor(stateid))



## The coefficient becomes -0.046 with a (conventional) SE of 0.019. 
## This is a large reduction in the coefficient from the pooled LS estimator, 
## suggesting that there was important omitted variable bias in the pooled LS estimate
(model_b_summary <- summary(model_b))



## (c)
## Do the results change when you add in fixed time effects? 
model_c <- lm(data = guns.data, 
              log(vio) ~ shall + incarc_rate + density + avginc + pop + 
                         pb1064 + pw1064 + pm1029 + factor(stateid)+ factor(year))



## The estimated coefficient now drops to -0.028 with a SE of 0.017. 
## This is a smaller effect and it is no longer significant at the 5% level,
(model_c_summary <- summary(model_c))
model_c_summary$coefficients[1:2, ]



## Now we can compute the F_stat
## model_c is the unrestricted model
## model_b is the restricted model
SSE_unre <- sum(resid(model_c) ^ 2)
SSE_re <- sum(resid(model_b) ^ 2)



## m is the number of restrictions
## K is the number of regressors in the unrestricted model
m <- length(model_c$coefficients) - length(model_b$coefficients)
K <- length(model_c$coefficients)



## F statisitc is 17.07473 which is highly significant, so adding time fixed effect is preferred
(F_stat_d <- ((SSE_re - SSE_unre) / m)/(SSE_unre / (nrow(guns.data) - K)))


