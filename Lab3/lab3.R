library(ISLR)
library(MASS)
library(class)
#(a) Produce some numerical and graphical summaries of the Weekly data. 
#Do there appear to be any patterns?
# No, I can't find any patters in the data
summary(Weekly)
#plot(Weekly)
pairs(Weekly)
#(b) Use the full data set to perform a logistic regression with Direction as the 
#response and the five lag variables plus Volume as predictors. Use the summary function 
#to print the results. Do any of the predictors appear to be statistically significant? 
#If so, which ones? Only Lag2 has some importance but it's kind of low with p = 0.0296
attach(Weekly)
glm = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data= Weekly,family = binomial)
summary(glm)

#(c) Compute the confusion matrix and overall fraction of correct predictions. 
#Explain what the confusion matrix is telling you about the types of mistakes 
#made by logistic regression
# You can see that 430/ 478 missclasifications happen in the "UP" class. 
predictions = predict(glm,type = "response")
contrasts(Direction) # remember to double check this
pred  = rep("Down",1089) #create a vector of N(number of observations) Downs
pred[predictions > 0.5] = "Up" # in my vector of N "Downs" every time 
table(pred,Direction) # Confusion matrix
mean(pred == Direction) #accuracy 


#(d) Now fit the logistic regression model using a training data period from 
#1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix 
#and the overall fraction of correct predictions for the held out data (that 
#is, the data from 2009 and 2010).
#newWeekly = Weekly[Year > 2008 & Year < 2011, ]
train = Year < 2009 # boolean vector representing the training data
glm0910 = glm(Direction~Lag2, data= Weekly[train,],family = binomial)
predictions0910 = predict(glm0910, newdata = Weekly[!train,],type = "response")
contrasts(Direction) # remember to double check this
pred0910  = rep("Down",104) #create a vector of N(number of observations) Downs
pred0910[predictions0910 > 0.5] = "Up" # in my vector of N "Downs" every time 
table(pred0910,Weekly[!train,]$Direction) # Confusion matrix
mean(pred0910 == Weekly[!train,]$Direction) #accuracy 


#(e) Repeat (d) using LDA. 
lda.fit = lda(Direction~Lag2, data= Weekly[train,])
lda.pred = predict(lda.fit,Weekly[!train,])
table(lda.pred$class,  Weekly[!train,]$Direction)
mean(lda.pred$class == Weekly[!train,]$Direction)

#(f) Repeat (d) using QDA
qda.fit = qda(Direction~Lag2, data= Weekly[train,])
qda.pred = predict(qda.fit,Weekly[!train,])
table(qda.pred$class,  Weekly[!train,]$Direction)
mean(qda.pred$class == Weekly[!train,]$Direction)
#(g) Repeat (d) using KNN with K = 1.
knn.train = cbind(Lag2,0)[train,]
knn.test = cbind(Lag2,0)[!train,]
knn.pred = knn(knn.train,knn.test,Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

#(h) Which of these methods appears to provide the best results on this data?
# Logistic Regression and LDA both provide the highest accuracy(62.5 %)

#(i) Experiment with different combinations of predictors, including possible
#transformations and interactions, for each of the methods. Report the variables, 
#method, and associated confusion matrix that appears to provide the best results 
#on the held out data. Note that you should also experiment with values for K in the KNN classifier.



#testing with differente values of k in knn
# I found the best results with k=4, which has an accuracy of 0.5769231
# After that value of k the accuracy flattens out. 
knn.train2 = cbind(Lag2,0)[train,]
knn.test2 = cbind(Lag2,0)[!train,]
knn.pred2 = knn(knn.train2,knn.test2,Direction[train],k=4)
table(knn.pred2,Direction[!train])
mean(knn.pred2==Direction[!train])


#Testing with different cases of Logistic regression
# Increasing the exponent in the Lag2 term won't produce any benefit
# I could not find and interaction terms that improves the model
glmTest = glm(Direction~Lag2 + Lag1*poly(Volume,2), data= Weekly[train,],family = binomial)
summary(glmTest)
predictionsTest = predict(glmTest, newdata = Weekly[!train,],type = "response")
predTest  = rep("Down",104) #create a vector of N(number of observations) Downs
predTest[predictionsTest > 0.5] = "Up" # in my vector of N "Downs" every time 
table(predTest,Weekly[!train,]$Direction) # Confusion matrix
mean(predTest == Weekly[!train,]$Direction) #accuracy

#Testing with different cases of LDA
# Increasing the exponent in the Lag2 term won't produce any benefit
# I could not find and interaction terms that improves the model
lda.fit2 = lda(Direction~Lag2 + Volume*Lag4, data= Weekly[train,])
lda.pred2 = predict(lda.fit2,Weekly[!train,])
table(lda.pred2$class,  Weekly[!train,]$Direction)
mean(lda.pred2$class == Weekly[!train,]$Direction)
