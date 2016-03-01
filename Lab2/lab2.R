#Setting workind directory?
setwd("~/RIT/KDD/Lab2")
#reading the file
auto <- read.csv("Auto.csv",header = TRUE,na.strings = "?")
# Part I starts here
#(a) Produce a scatterplot matrix, which includes all of the variables in the data set.
plot(auto)
#(b) Compute the matrix of correlations between the variables using the function cor()
#rownames(auto) = auto[,9]
auto = auto[,-9]
cor(auto)
#(c)Use the lm() function to perform a multiple linear regression with mpg as the
#response and all other variables except name as the predictors. Use the summary() 
#function to print the results.
linearModel <- lm(mpg~.,data = auto)
summary(linearModel)
#i. Is there a relationship between the predictors and the response?
#   Yes, there is

# ii. Which predictors appear to have a statistically significant relationship to the response?
# Weight, Year ,  Origin and Displacement.

# iii. What does the coefficient for the year variable suggest?
# That as the year increases the mpg also increases. There's a positive relationship.


#(d) Use the plot() function to produce diagnostic plots of the linear regression fit. 
#Comment on any problems you see with the fit. Do the residual plots suggest any unusually 
#large outliers? Does the leverage plot identify any observations with unusually high leverage?
plot(linearModel)
# I think the main problem is that the data follows some kind of pattern, in other words the distrubution
# of data points doesn't look random(kind U shape), this suggest that the relation might not be linear.
# Yes, looking at the graph there are some point that have a much greater residual than the average

# Yes, for exaple data point #14 has a unsually high Cook's distance.

#(e) Use the * and : symbols to fit linear regression models with interaction effects. 
#Do any interactions appear to be statistically significant?
#lm.fitc = lm(Sales~.Income:Advertaising+Price:Age,data=Carseats)
attach(auto)
lmInteraction = lm(mpg~weight*year*origin,data= auto)
#lmInteraction = lm(mpg~weight*year*cylinders,data= auto)
#lmInteraction = lm(mpg~weight*year*displacement,data= auto)
summary(lmInteraction)
# Yes, this ones are statistically important:weight:year, weight:origin,year:origin,weight:year:origin 

#(f) Try a few different transformations of the variables, such as log(X), X0.5, X2. 
#Comment on your findings.
summary(lm(mpg~I(weight^2)))
summary(lm(year~I(horsepower^2)))
summary(lm(year~I(mpg^2)))
summary(lm(mpg~I(weight^0.5)))
summary(lm(weight~log(mpg)))
summary(lm(cylinders~poly(displacement,3)))
# All of the non-linar transformations yielded terms of statistically significance. This suggest a non
# Linear relation of the terms.

        