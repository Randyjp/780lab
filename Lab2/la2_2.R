#import ISRL packege
#install.packages("ISLR")
library(ISLR)
#(a) Fit a multiple regression model to predict Sales using Price, Urban, and US.
names(Carseats)
attach(Carseats)
lmfit <- lm(Sales~Price+Urban+US,data=Carseats)
summary(lmfit)
# Price: as the price increaes the sales descrease.
# UrbanYes: This is a quantitave predictor that takes the values of Yes and No. The model takes the Yes as
# the base line. So when Urban is yes the Sales decrease.
# USYes: Same as the previous one. When the US is yes the sales increase.

#(c) Write out the model in equation form, being careful to handle the qualitative
#variables properly.
# yi = Bo + B1 + B1Xi
# where X1 = 1 if USA = yes 
# and X1= 0 if USA = no

#(d) For which of the predictors can you reject the null hypothesis H0: βj = 0?
#   For urban

#(e) On the basis of your response to the previous question, fit a smaller model that
#only uses the predictors for which there is evidence of association with the outcome.
lmfit2 <- lm(Sales~Price+US,data=Carseats)
summary(lmfit2)

#(f) How well do the models in (a) and (e) fit the data?
# a) Residual standard error: 2.472  Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2335 
# e) Residual standard error: 2.469 Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2354 
# The results are actually very similar, the e model is slighly better having a smallar RSE

#(g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
confint(lmfit2,level=0.95)
