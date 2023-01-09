rm(list = ls())
setwd('/Users/lexidingle/Desktop')

batch2 <- read.csv('batch2.csv', stringsAsFactors = F )
str(batch2)
head(batch2)
# logistic regression for starting salary based on region 
lm.fit <- lm(salary ~ yearsExperience + jobType + industry, data = batch2)

s1 = batch2[1:2000, ]

s1.matrix = as.data.frame(data.matrix(s1))

plot(s1.matrix$yearsExperience, s1.matrix$salary, col=s1.matrix$jobType)

boxplot(salary ~ yearsExperience, data = s1, main = 'Salaries based on Years of Experience',
        xlab = 'Years of Experience', ylab = 'Salary')
boxplot(salary ~ jobType, data = s1, main = 'Salaries based on Job Type',
        xlab = 'Job Type', ylab = 'Salary', col = (c('red', 'blue')))
boxplot(salary ~ industry, data = s1, main = 'Salaries based on Industry',
        xlab = 'Industry', ylab = 'Salary', col = (c('red', 'blue')))


plot(salary ~ yearsExperience, data = s1)
plot(s1$salary, s1$yearsExperience)
summary(lm.fit)


# coefficients 
coef(lm.fit)
summary(lm.fit)$coef

# histogram of data 
hist(batch2$salary)

# box plot 

boxplot(batch2$salary)

# p-values 
summary(lm.fit)$coef[,4]

glm.probs <- predict(lm.fit, type = 'response')
glm.probs[1:10]

# create test and train 
train = sample(nrow(batch2), nrow(batch2) * .6)

batch.train = batch2[train, ]
batch.test = batch2[-train, ]

# cross validation -- mean squared error 
set.seed(300)
lm.fit <- lm(salary ~ yearsExperience + jobType + industry, data = batch2, 
             subset = train)
lm.fit <- lm(salary ~ yearsExperience + jobType + industry, data = batch2[train, ])
mean((batch2$salary - predict(lm.fit, batch2))[-train]^2)

#MSE 
# 625.3022



# 10-fold cross validation
library(boot)
set.seed(300)
lm.fit <- lm(salary ~ yearsExperience, data = batch2)
cv.error <- cv.glm(batch2, lm.fit, K = 10)$delta[1]


# dummy variables 
length(unique(batch2$jobType))
batch2$dummyjob <- ifelse(batch2$jobType == "CFO", 1, 0)

#scatter plot 
plot(x = batch2$,y = batch2$salary,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(2.5,5),
     ylim = c(15,30),		 
     main = "Weight vs Milage"
)

# graphing ideas 
library(car)
avPlots(lm.fit)


unique(batch2$industry)


