# a) Loading the data and naming it college
setwd("~/RIT/KDD/Lab1")
college = read.csv("College.csv",header = T)
fix(college)
# b) creating a rownames column, this takes the first column and creates the new column with that data. R will only
#use it as an indetifier and will not perform caculations with it.
rownames(college)=college[,1]
fix(college)
#Delete the first row given tha fact that we already have than info in rownames
college = college[,-1]
fix(college)

# c) numerical summary
summary(college)
# Production a scatter plot matrix but only selecting the first 10 columns
pairs(college[,1:10])

#side by side boxplots of Outstate vs Private
attach(college)
plot(Private,Outstate)

#Create a qualitvative var saying if top students(10% of their highschool) are more than 50%
Elite = rep("No",nrow(college))
Elite[Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college,Elite)

summary(college)
plot(Elite,Outstate)
#Drawing some histgrams
par(mfrow=c(2,2)) # divide the screen into 4
hist(PhD,plot = TRUE)
hist(Apps,plot = TRUE)
hist(Outstate,plot = TRUE)
hist(Expend,plot = TRUE)

# v) Private is qualitative. The rest of the predictos are quantitative
range(Enroll) #35-6392
mean(Enroll) #779.973
sd(Enroll) #929.1762

#removing observations 100-200
trimEnroll = Enroll[-(100:200)]
trimEnroll
range(trimEnroll) #35-6392
mean(trimEnroll) #823.0695
sd(trimEnroll) #972.2258
