library(ISLR)
require(tree)
#install.packages("randomForest")
require(randomForest)
#1. Split the data set into a training set and a test set.
attach(Carseats)
set.seed(20)
train = sample(1:nrow(Carseats),300) #get 300 for trainning

#2. Fit a regression tree to the training set. Plot the tree, and interpret the results.
#What test MSE do you obtain?
car.tree = tree(Sales~., data = Carseats[train,])
plot(car.tree)
text(car.tree, pretty = 0)
summary(car.tree)
predTree  = predict(car.tree, newdata = Carseats[-train,])
mean( (predTree - Carseats[-train,]$Sales)^2 ) #test MSE

#3. Use cross-validation in order to determine the optimal level of tree complexity.
# Does pruning the tree improve the test MSE? NO
cv.carTree = cv.tree(car.tree, FUN = prune.tree)
plot(cv.carTree)
cv.carTree
#prune.tree  = prune.tree(car.tree, best = 15)
prune.tree  = prune.tree(car.tree, best = 8)
predPrune  = predict(prune.tree, newdata = Carseats[-train,])
mean( (predPrune - Carseats[-train,]$Sales)^2 ) #test MSE

#4. Use the bagging approach in order to analyze this data. What test MSE do you 
#obtain? Use the importance() function to determine which variables are most important
bg.cars = randomForest(Sales~., data = Carseats[train,], importance = TRUE, mtry = 10)
bg.cars$importance
bg.pred = predict(bg.cars, newdata = Carseats[-train,])
mean((bg.pred - Carseats[-train,]$Sales)^2)
#5. Use random forests to analyze this data. What test MSE do you obtain? Use the 
#importance() function to determine which variables are most important. Describe the
#effect of m, the number of variables considered at each split, on the error rate obtained.
rf.cars = randomForest(Sales~., data = Carseats[train,], mtry = 4 ,importance = TRUE)
rf.cars$importance
rf.pred = predict(rf.cars, newdata = Carseats[-train,])
mean((rf.pred - Carseats[-train,]$Sales)^2)
# By selecting a differente number of variables for the splits random forest manages to decorrale the trees
# this results in a lower error. 