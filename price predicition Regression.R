	setwd(~Desktop)
car <- read.csv('~/Desktop/cardata2.csv')

# regression predicting price using type 

priceType <- lm(Price ~ Type, data = car )
summary(priceType)

= 40832 + (- 26661H ) +(-23105 C) + (-19764 S) + (-17972 W )

hachback = 14171
---------------------------------------------------------------------------------------------
# regression for price based on type and mileage 

priceTypeMil <- lm(Price ~ Type + Mileage, data = car )
summary(priceTypeMil)

convertible & 20000 miles = 4.489 + (-1.960 * 20000)

-------------------------------------------------------------------------------------
# regression predicting log(Price) based on Doors and Cylinders 

priceDoor <- lm(log(Price) ~ Doors + Cylinder, data = car )
summary(priceDoor)
= 9.128446 + (-0.044797 * Doors) + (  0.172457 * Cylinder )
= 9.128446 + (-0.044797 * 2) + (  0.172457 * 8 )

a car with 2 doors and 8 cylinders: 10.41851

-------------------------------------------------------------------------------------


# interactions 

#no interaction 
summary( lm( Price ~ Mileage * Liter, data = car ) )

= 3669.79882 + ( 0.13038 * Mileage ) + (6844.85522 * Liter) + (-0.09493 * Mileage * Liters)


# same as 
C$Mileage_x_Liter <- C$Mileage * C$Liter 
summary( lm( Price ~ Mileage + Liter + Mileage_x_liter, data = car ) )


------------------------------------
# predict price based on depth and able 

summary( lm( price ~ depth * table, data = D ) )

= 20946.118 + (-503.030 * Depth) + (-371.228 * table) + (9.974 * table * depth)

# for 60 depth and 60 table 

20946.118 + (-503.030 * 60) + (-371.228 * 60) + (9.974 * 60 * 60)

= 4397.038 


# now with interaction 

D$depth_x_table <- D$depth * D$table
summary( lm(price ~ depth + table + depth_x_table, data = D))

= 20946.118 + (-503.030 * depth) + (-371.228 * table ) + ( 9.974 * depth * table)
-----------------------------------

# car: predict price using mileage and leather 
#.... no interaction 
# binary and categorical variables 


summary( lm(Price ~ Mileage + Leather , data = car ))

= 2.225e+04 + ( -1.727e-01 * Mileage ) + (3.477e+03 * leather)

# .... interaction 

summary( lm(Price ~ Mileage * Leather , data = car ))

2.266e+04 + (-1.933e-01 * Mileage) + ( 2.925e+03 * Leather) + (2.781e-02 * Mileage * Leather )
 
# the slope is not longer equal, use interaction to use patterns of data to see the influence of an outcome depending on another variable, the amount of the effect of leather on a car depends on the mileage 


#takes many strings and creates one string 
paste('1','2','3')
-> '1 2 3'


age.data <- data.frame( age = c(27,30,32,33,35,40,44,45,50,58,59,60), buyer = c(0,0,1,0,0,1,1,1,0,1,1,1) )

setwd('/Users/lexidingle/Desktop')
car <- read.csv('cardata2.csv')

nick<- subset(cars, nickspicks= 1)

#subest of NewCars
mod.2 <- lm(Price ~ Mileage, data = Newcar)



lm( Price ~ Mileage * Make, data = car)


Price = B0 + BM*Mileage + BP*DP + Bint*Mileage*DP 
#DP will be 1 since it is just if it is a pontiac 

#create this data frame 

age.data <- data.frame( age = c(27,30,32,33,35,40,44,45,50,58,59,60), buyer = c(0,0,1,0,0,1,1,1,0,1,1,1) )

#linear regression
summary( lm( buyer~age, data = age.data))

#LOGISTIC REGRESSION 
# never can go below zero , is an s curve , harder to interpret 
summary( glm(buyer~age, family = binomial , data = age.data ) )

predicted probability = exp(regression equation) / 1+ exp(regression equation)
predicted probability = exp(B0 + B1*age)/ 1+ exp( B0 + B1*age)
exp(-5.48178 +  0.14230*AGE)/  (1+exp(-5.48178 +  0.14230*AGE))
exp(-5.48178 +  0.14230*40)/  (1+exp(-5.48178 +  0.14230*40))

# there is a 55% chance a 40 year old makes a purchase 

-----------------------------------------------------------------------------------------------------
  # are more expensive cars more likely to have leather interior 
  #solved with a linear model 
  summary(lm(leather ~ price , data = car))

# logistic model 
summary(glm( Leather ~ Price, family = binomial , data = car))

exp(8.925e-02 + 4.288e-05 * Price) / (1+ exp(8.925e-02 + 4.288e-05 * Price))

exp(8.925e-02 + 4.288e-05 * 10000) / (1+ exp(8.925e-02 + 4.288e-05 * 10000))
# 63% likelihood that a car worth $10000 will have leather 

------------------------------------------------------------------------------------------------------
  # add another predictor 
  summary( glm( Leather ~ Price + Mileage, family = binomial, data= car ) )
exp(8.925e-02 + 4.288e-05 * Price + ) / (1+ exp(8.925e-02 + 4.288e-05 * Price))

# Question 8 
apple$Predicted 
apple <- setwd('/Users/lexidingle/Desktop')

#change the Yes and No column to logic so 1 and 0 are expressed 
apple$Newbuy<- as.numeric(apple$Buy =='Yes' )

summary( glm( Buy ~ Income, family = binomial, data = apple) )
exp(B0 + B1*income)/ 1+ exp( B0 + B1*income)
exp(-1.991e+00 + 2.914e-05*50000)/ 1+ exp( -1.991e+00 + 2.914e-05*50000)
# 36 % chance that they purchase if they have an income of 50000




------------------------------------------------------------------------------------------------------
  
  
  
  summary( pmod <- lm( Price ~ Mileage * Make, data = car ))

2.399e+04 + ( -1.556e-01* Mileage) + (-3.769e+03 * MakePontiac) + (2.296e+04* Makecadillac ) + (6.180e-02* mileage * make pontiac) + (-1.628e-01* mileage* makecadillac)



pontiac = 2.399e+04 + ( -1.556e-01* 10000) +(-3.769e+03 * 1) + (6.180e-02* 10000 * 1) 

Pontiac = 19283

y intercept = 2.399e+04 + (-3.769e+03 * 1) 
20221
# this is if there are zero miles on a pontiac 


cadillac = 2.399e+04 + (- 1.556e-01* 10000)  + (2.296e+04* 1 ) +(-1.628e-01* 10000* 1)

price with 10000 = 43766


2.399e+04  + (2.296e+04* 1 )
y intercept = 46950 














