#Author- Lavanya Jain

#Importing Data
library(readxl)
TSF_TASK_1 <- read_excel("C:/Users/user/OneDrive/Desktop/TSF TASK 1.xlsx")
View(TSF_TASK_1)

#GRAPH
plot(TSF_TASK_1$Hours,TSF_TASK_1$Scores, main="Percentage Scores Vs Hours Studied",
     xlab="Hours studied", ylab="Percentage Scores")
#PREDICTION USING LINEAR REGRESSION MODEL (Using whole data set)
linmod = lm(Scores ~ Hours, data = TSF_TASK_1)
summary(linmod)
abline(linmod, col= "red") #Fitting regression line
predict(linmod, data.frame(Hours = 9.25)) #prediction using original data

#PREDICTION USING TRAINING AND TEST DATA
library(caTools)
set.seed(123)
sample = sample.split(TSF_TASK_1$Scores,SplitRatio = 0.8) 
train1 =subset(TSF_TASK_1,sample ==TRUE)
test1 =subset(TSF_TASK_1,sample ==FALSE)
trainmod = lm(Scores ~ Hours, data = train1)
summary(trainmod)
pred = predict(trainmod,test1)
print(pred) #predicted values
print(test1) #actual values
predict(trainmod, data.frame(Hours = 9.25)) #prediction using training data

