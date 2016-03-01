#libraries
#install.packages("glmnet")
library(glmnet)
#set working directory
setwd("~/RIT/KDD/Lab4")
#read data
college = read.csv("College.csv", header = TRUE,na.strings = "?")
college = college[,-1]

attach(college)
#1. Split the data set into a training set and a test set.
train = college[1:500,]
test = college[501:777,]

#2. Fit a linear model using least squares on the training set, and report the test error
#obtained.
lm.fit = lm(Apps~ ., data = train)
pred = predict(lm.fit,newdata = test, se.fit = TRUE)
summary(lm.fit)
mean((pred$fit - test$Apps)^2) #error of predictions.
pred$residual.scale #standard 
#3)Fit a ridge regression model on the training set, with λ chosen by cross-validation.
#Report the test error obtained
#ridge = glmnet()
x = model.matrix(Apps~.-1, data = train) #if you type what you want to predict and put a -1 it removes it form the model
y = train$Apps # response or what I want to predict
ridge = glmnet(x, y, alpha = 0)
plot(ridge, xvar = "lambda", label = TRUE) # checking the coefficients
cv.ridge = cv.glmnet(x, y, alpha=0)
plot(cv.ridge)
bestLambda = cv.ridge$lambda.min # getting best lambda
ridge.pred = predict(ridge, s= bestLambda , newx = model.matrix(Apps~.-1, data = test))
mean((ridge.pred- test$Apps)^2) 

#4. Fit a lasso model on the training set, with λ chosen by cross-validation. Report the
#test error obtained, along with the number of non-zero coefficient estimates.
lasso = glmnet(x, y, alpha = 1)
plot(lasso, xvar = "lambda", label = TRUE) # checking the coefficients
set.seed(1)
cv.lasso = cv.glmnet(x, y, alpha=1)
plot(cv.lasso)
bestLambda2 = cv.lasso$lambda.min 
lasso.pred = predict(lasso, s= bestLambda2 ,newx = model.matrix(Apps~.-1, data = test))
mean((lasso.pred- test$Apps)^2) 

#5. Comment on the results obtained. How accurately can we predict the number of college 
#applications received? Is there much difference among the test errors resulting from these five approaches?

#This lasso regression provies the smaller error and also is not using all the predictors, given the fact tha
# | Bj | can make coefficiente zero when lambda is big enough, so we end up using only 4 predictors.

