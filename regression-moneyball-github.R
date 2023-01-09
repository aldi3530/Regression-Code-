# moneyball exercise
# Dan Zhang, August 19, 2016

rm(list=ls())
# Change the directory to the one on your computer
# setwd("D:/Dropbox/courses/DataAnalytics/exercises/moneyball")
mlb <- read.csv("https://www.dropbox.com/s/k14bjmtrth7pl6m/mlb11.csv?dl=1")

str(mlb)
summary(mlb)
pairs(mlb[,-1])

# find pairwise correlation
# alternatively, move the first column to row.names
cor(mlb[,-1])

# alternative way
row.names(mlb) <- mlb$team
mlb <- mlb[,-1]
cor(mlb)

# linear regression model
lm.fit <- lm(runs~at_bats,data=mlb)
summary(lm.fit)

# check linearity using scatter plot and residual plot
plot(mlb$at_bats, mlb$runs)
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(lm.fit)
par(mfrow=c(1,1)) # Change back to 1 x 1

# check normality
qqnorm(lm.fit$residuals)
# constant variability - look at scatter plot

# the variable new_obs (the sum of on-base percentage and slugging percentage) 
# has the highest correlation with runs
lm.fit1 <- lm(runs~new_obs,data=mlb)
summary(lm.fit1)
plot(mlb$new_obs, mlb$runs)
qqnorm(lm.fit1$residuals)


