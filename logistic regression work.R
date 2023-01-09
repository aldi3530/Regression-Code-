# logistic regression 



install.packages('caTools')
set.seed(200)
rm(list = ls())
setwd('/Users/lexidingle/Desktop')

install.packages('caTools')
library(caTools)
set.seed(200)


# read data Please download Appointments.csv
appt <- read.csv("appointments.csv", stringsAsFactors = TRUE)
str(appt)

train <- sample.split(appt$Status, SplitRatio = 0.6)
table(train, appt$Status)

appt.test <- appt[-train, ]


glm.train <- glm(Status ~ . - MRN, data = appt, subset = train, 
                 family = binomial)

# type = reponse give probabilities 

cancel.prob <- predict(glm.train, newdata = appt.test, type = "response")
cancel.pred <- as.numeric(cancel.prob > 0.2) # threshold value is 0.2
table(appt.test$Status, cancel.pred)

?predict

(658 + 217) * 5 
 
217 * .3 * 100
6510 - 4375


