library(ISLR)
library(MASS)
library(leaps)
summary(Boston)
attach(Boston)
library(glmnet)
####### Predict 
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id=id)
  mat[,names(coefi)]%*%coefi
}

#########


#10-fold cross validation with best subset apporach
set.seed(11)
folds = sample(rep(1:10,length = nrow(Boston)))
cv.errors = matrix(NA,10,13)
#best subset
for(k in 1:10){
  regfit.full = regsubsets(crim~., data = Boston[folds != k,], nvmax = 13)
  for(i in 1:13){
    pred = predict(regfit.full, Boston[folds==k,],id=i)
    cv.errors[k,i] = mean((Boston$crim[folds==k] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=13, type="b")
rmse.cv[9] #lowest error = 6.616255 with model of size 9

#regfit.full = regsubsets(crim~., data = Boston[train,], nvmax = 13)
#reg.summary = summary(regfit.full)
#plot(reg.summary$cp, xlab = "Number of Variables" , ylab = "Cp") #Graph of CP against number of vars
#which.min(reg.summary$cp)

#plot(reg.summary$adjr2, xlab = "Number of Variables" , ylab = "adj R^2") #Graph of CP against number of vars
#which.max(reg.summary$adjr2)
#coef(regfit.full,10)


#10-fold cv with forward stepwise
train=sample(seq(506),338, replace = FALSE)
for(k in 1:10){
  fwd = regsubsets(crim~., data = Boston[train,], nvmax = 13, method = "forward")
  for(i in 1:13){
    pred = predict(fwd, Boston[folds==k,],id=i)
    cv.errors[k,i] = mean((Boston$crim[folds==k] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=13, type="b")
rmse.cv[10]

#fwd = regsubsets(crim~., data = Boston[train,], nvmax = 13, method = "forward")
#fwd.summary = summary(fwd)
#plot(fwd, scale = "Cp")
#plot(fwd.summary$cp, xlab = "Number of Variables" , ylab = "Cp") #Graph of CP against number of vars
#which.min(reg.summary$cp)


#lasso
#train=sample(seq(506),338, replace = FALSE)
x = model.matrix(crim~., data = Boston)
y = Boston$crim
lasso = glmnet(x[train,], y[train], alpha = 1)
plot(lasso, xvar = "lambda", label = TRUE) # checking the coefficients
set.seed(1)
cv.lasso = cv.glmnet(x, y, alpha=1, keep = TRUE)
plot(cv.lasso)
bestLambda2 = cv.lasso$lambda.min
lasso.pred = predict(lasso, s= bestLambda2  ,newx = x[-train,])
mean((lasso.pred- y[-train])^2) 

#Does your chosen model involve all of the features in the dataset? Why or why not?
#it onl includes 9 out of 13. This is a simpler model that can explain a good amount of variance
# adding more variables would only slighly improve. So you always choose the simple model to control
# the variance of the bias-variace tradeoff 

