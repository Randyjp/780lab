library(ISLR)
#install.packages("tree")
require(tree)
#1. Create a training set containing a random sample of 800 observations, and a test
#set containing the remaining observations.
attach(OJ)
set.seed(1)
train = sample(seq(nrow(OJ)), 800, replace = FALSE)

#2. Fit a tree to the training data, with Purchase as the response and the other
#variables except for Buy as predictors. Use the summary() function to produce summary 
#statistics about the tree, and describe the results obtained. What is the training 
#error rate? How many terminal nodes does the tree have?
tree.oj = tree(Purchase~., data = OJ[train,]) #fitting a tree molde
summary(tree.oj)
# The tree only used 3 variables(LoyalCH, PriceDiff, ListPriceDiff) and the root is splited by LoyalCH
# Number of terminal nodes:  8 
# Misclassification error rate: 0.165 = 132 / 800 

#3. Type in the name of the tree object in order to get a detailed text output. Pick one of the terminal 
# nodes, and interpret the information displayed.
tree.oj
# 11) PriceDiff > 0.065 103  140.00 CH ( 0.58252 0.41748 ) *

#4. Create a plot of the tree, and interpret the results.
plot(tree.oj)
text(tree.oj, pretty= 0)

#5. Predict the response on the test data, and produce a confusion matrix comparing
#the test labels to the predicted test labels. What is the test error rate?
treePred = predict(tree.oj, newdata = OJ[-train,], type = "class")
table(treePred, OJ[-train,]$Purchase) #confusion Matrix
mean(treePred == OJ[-train,]$Purchase) # test error

#6. Apply the cv.tree() function to the training set in order to determine the optimal
#tree size.
cv.oj = cv.tree(tree.oj, FUN = prune.misclass) #prune using missclassification
cv.oj
#7. Produce a plot with tree size on the x -axis and cross-validated classification error
#rate on the y -axis.
plot(cv.oj)

#8. Which tree size corresponds to the lowest cross-validated classification error rate?
# all trees of size 2- 8 

# 9. Produce a pruned tree corresponding to the optimal tree size obtained using cross-
# validation. If cross-validation does not lead to selection of a pruned tree, then
# create a pruned tree with five terminal nodes.
prune.oj = prune.misclass(tree.oj, best = 5)
text(prune.oj, pretty = 0)

#Compare the training error rates between the pruned and un-pruned trees. Which
#is higher? It's the same.
summary(prune.oj)
#Misclassification error rate: 0.165 = 132 / 800

#11. Compare the test error rates between the pruned and un-pruned trees. Which is
#higher? I got the same results
prunedPred = predict(prune.oj, newdata = OJ[-train,], type="class")
table(prunedPred, OJ[-train,]$Purchase) #confusion Matrix
mean(prunedPred == OJ[-train,]$Purchase) # test error
