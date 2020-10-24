## This R script completes the programming part of PS6 Q4
## Created by Alex Li



## The following line of code only needs to be run once
## install.packages(c("utils", "dplyr"))
## Import the packages used in this assignment
library(utils)
library(dplyr) 



## This problem set requires you to do a robust standard-error
## To do this, we need two more packages called "multiwayvcov" and "lmtest"
## It contains linear models for panel data estimated using the lm function on transformed data.
install.packages(c("multiwayvcov", "lmtest"))
library(multiwayvcov)
library(lmtest)



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



## (1) Clubster by state
## Now do a fixed effect regression, 
## With the same variables as in (ii), 
## but including state fixed effects 
## How does this affect the size and significance of the coefficient on shall?
# estimate the fixed effects regression with plm()
model_b <- lm(data = guns.data, 
              log(vio) ~ shall + incarc_rate + density + avginc + pop + 
                         pb1064 + pw1064 + pm1029 + factor(stateid))



vcov_state <- cluster.vcov(model_b, guns.data$stateid)
model_b_robust <- coeftest(model_b, vcov_state)
model_b_robust[1:2,]



## For comparison
model_b_summary <- summary(model_b)
model_b_summary$coefficients[1:2,]
## Notice that the estimate for the coeffient didn't change (remained as -0.0461)
## The standard error increased from 0.01886682 to 0.04268855 when we use clustered robust SE.



## (2) Cluster by state and year
## Add time fixed effects
model_c <- lm(data = guns.data, 
              log(vio) ~ shall + incarc_rate + density + avginc + pop + 
                pb1064 + pw1064 + pm1029 + factor(stateid)+ factor(year))



vcov_stateyear <- cluster.vcov(model_c, cbind(guns.data$stateid, guns.data$year))
model_c_robust <- coeftest(model_c, vcov_stateyear)
model_c_robust[1:2,]



## For comparison
model_c_summary <- summary(model_c)
model_c_summary$coefficients[1:2,]
## Notice that the estimate for the coeffient didn't change (remained as -0.02799361)
## The standard error increased from 0.01715784 to 0.04639385 when we use clustered robust SE.



## (3) Impications
## Thus using robust SEs results in the estimated e§ects becoming insigniÖ- cantly di§erent from zero.