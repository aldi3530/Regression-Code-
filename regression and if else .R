#Regression and If Else statements 


# univariate regression (regression with one predicter variables )

# multivariate regression( multiple predictor variables )

#diamonds data
require(ggplot2)
D<- as.data.frame(diamonds)


#Diamonds: predict prive using weight 

lm( Y ~ X , data = D )

mylm <- lm(price ~ carat, data = D)

coef( mylm ) [1] 

# ususlly use 
summary( mylm ) 

#residuals are how far off you are from mean 

mean( mylm$resid )


# r-squared is how much is explained by the model 



#regression equation created from summary information 
# regression model = price = B + B x carat 

price = (-2256) + 7756 * carat 
(-2256) + 7756 * .2  





#car data 

setwd(~Desktop)
car <- read.csv('~/Desktop/cardata2.csv')


carinfo <- lm(Price ~ Mileage, data = car )
summary(carinfo)

price = 24760 + (-.17) * mileage





# writing a regression line 
summary ( lm( log ( price ) ~ log (carat ), data = D ))

price = 8.44 + 1.68 * carats 
## we are now pluggin in log(carats) and recieving log(price)

# in order to recieve log we need to raise e






# Multivariate regression predict carat using x, y, z 

summary(lm(carat ~ x + y + z, data = D))

carat = B + Bx + By + Bz 
carat = -1.566773 + .359054x + .005154y + .078378z


cor( D$x, D$y )
# these two variables are to highly correlated that causes a weird output



#used car multivariate 

summary( lm( Price ~ Mileage + Liter, data = car))

carprice = 9426.60 + (- .16003)x + 4968.28y

9426.60 + (- .16003*15000) + (4968.28*3.1)


#binary predictor in Regression 

#based on whether or not there is a variable 

# equation 
x = B + B * dummy variable 

# predict price for car based on whether or not the car has leather interior

summary( lm(Price ~ Leather, data = car))

price = 18828.8 + 3473.5 * D(leather)
price = 18828.8 + 3473.5 * 1


#will give an average for cars with leather then without 
aggregate(Price ~ Leather , data = car , FUN = mean)

t.test(car$Price ~ car$Leather, var.equal = TRUE )


# run regression preduccting Price based on whether there is leather interior, 
# cruise control, and no sounds system ( no interaction )



summary(lm ( Price ~ Leather + Sound + Cruise, data = car))

Price = 12245.5 + 4585.5* (leather) + (- 2514.3*Sound) + 9948.1*(Cruise)


Price = 12245.5 + 4585.5* 1 + (- 2514.3*0) + 9948.1*1
#correlations can cause it to be hard to see 

cor(C$Leather, C$Sound)

#Run regression with categorical predictors with one that two options 
#predict price from cut 
#defactor cut 

D$cut <- as.character(D$cut)

summary(lm(price ~ cut, data = D ))

Price = B + B1 + B2 + B3 + B4 

price = 4358 + (-429* Good) + (-901 * Ideal) + 226*Premium + (-377*VeryGood)


D$dg <- ifelse( D$cut == 'Good', 1, 0)


D$di <- ifelse( D$cut == 'Ideal', 1, 0)

D$dp <- ifelse( D$cut == 'Premium', 1, 0)

D$dvg <- ifelse( D$cut == 'Very Good', 1, 0)


# redo equation 
summary(lm(price ~ di, dp, dvg, data = D ))




------------------------------------------------------------------------------
  # regression 
  setwd('/Users/lexidingle/Desktop')
fish <- read.csv('fish_hw4.csv')

summary( lm( Weight ~ Height + Width, data = fish))

= -433.576  + (4.825 * Height ) + (178.523 * Width)
-433.576  + (4.825 * 10 ) + (178.523 * 5)

---------------------------------------------------------------------

  
  summary( lm( Weight ~ Length1 * Length2 , data = fish) )
= -224.21645 + (-167.08932 * length1) + (165.38102 * length2 ) + (0.36228 * length1 * length2 )

-224.21645 + (-167.08932 * 20) + (165.38102 * 25 ) + (0.36228 * 20 * 25 )

------------------------------------------------------------------------------- 

  
  summary( lm( log(Length1) ~ log(Width), data = fish))

= 2.07902 + ( 0.79807 * Width)
2.07902 + ( 0.79807 * 8)
= exp(8.46358)
-------------------------------------------------------------------------------
  apple$Prediction <- ifelse(apple$Buy == 'Yes', 1, 0 )

summary( glm( Prediction ~ Income, family = binomial(link = 'logit'), data = apple))

exp(-1.991e+00 + 2.914e-05*50000) / (1 + exp(-1.991e+00 + 2.914e-05*50000))






















