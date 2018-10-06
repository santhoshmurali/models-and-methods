# Logistic Regression on CellPone - Group Assignment
# Ref : https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# Ref : https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/
# Ref : https://en.wikipedia.org/wiki/Generalized_linear_model#Link_function
# ref : http://www.statmethods.net/advgraphs/correlograms.html

#Required Libraries
library(googleVis)
library(outliers)
library(corrgram)
library(lmtest)
library(pscl)
library(AICcmodavg)
library(ROCR)




getwd()
setwd("C:/Home/Work/Data Science/models-and-methods")
mydata = read.table(file = "Logit/Data/CellPhone.csv", header = T, sep = ",")
names(mydata)[1] = "Churn"
sapply(mydata, function(x) summary(x))
scaledMyData = scale(mydata[,-1])

#Splitting the data as 70% Trainng and 30% Testing
training_index_sample = sample(nrow(mydata), nrow(mydata)*.7)
mydata_train = mydata[training_index_sample,]
mydata_test  = mydata[-training_index_sample,]

#Outliers

outlier(mydata)
#Let us find the corellation between independant variables
corraltedData = as.data.frame(cor(mydata[,-1]))
plot(gvisTable(corraltedData))
write.csv(corraltedData,file = "Logit/Data/corData.csv", row.names = TRUE, col.names = TRUE)
#Visual Representation of Correlation
install.packages("corrgram")

corrgram(mydata[,-1], order = TRUE, lower.panel = panel.pie, upper.panel = panel.pie, text.panel = panel.txt, main="Correlation Matirx", diag.panel = panel.minmax)
#Highly correlated variables
# Data Plan X Data Usage
# Data Plan X Monthly Charge
# Monthly Charge X DayMins
# Monthly Charge X OverageMins

#Logistic Regression based on additive effect
logitModel = glm(Churn ~ .,data = mydata_train, family = binomial)
summary(logitModel)
anova(logitModel)
#Testing Goodness of fit 

lrtest(logitModel)

pR2(logitModel)
confint(logitModel)

 logitModelTest = predict(logitModel, newdata = mydata_test, type = "response")
table(cut(logitModelTest,5))
ChurnPredicted = ifelse(logitModelTest>0.5,1,0)
mydata_test$PredictedChurn = ChurnPredicted

#Perfromnce of Logistic regression model
# AIC
#1. AIC (Akaike Information Criteria) – 
#The analogous metric of adjusted R² in logistic regression is AIC. AIC is the measure of 
#fit which penalizes model for the number of model coefficients. Therefore, we always 
#prefer model with minimum AIC value.
ifelse(any(grepl("AICcmodavg", installed.packages())),print("AICcmodavg Installed"),install.packages("AICcmodavg"))
AICc(logitModel, k=2)

#Confusion Matrix
table(mydata_test$Churn, mydata_test$PredictedChurn)

#ROC Curve: Receiver Operating Characteristic(ROC) 
install.packages("ROCR")

ROCRPred = ROCR::prediction(mydata_test$PredictedChurn, mydata_test$Churn)
ROCRPerf = ROCR::performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize = T)

library(ggplot2)
names(mydata_train)
ggplot(mydata_train, aes(AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins,Churn)) + geom_point() + stat_smooth(method="glm", family="binomial", se=FALSE)



#Logistic Regression based on interactive effect
logitModelInteraction = glm(Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins+DataPlan*DataUsage+DataPlan*MonthlyCharge+MonthlyCharge*DayMins+MonthlyCharge*OverageFee,                                data = mydata_train, family = binomial)
summary(logitModelInteraction)
anova(logitModelInteraction)
lrtest(logitModelInteraction)
pR2(logitModelInteraction)


logitModelInteractionTest = predict(logitModelInteraction,newdata = mydata_test, type = "response")
ChurnPredictedIntEf = ifelse(logitModelInteractionTest>.5,1,0)
mydata_test$PredictedChurnIntEf = ChurnPredictedIntEf
table(mydata_test$Churn, mydata_test$PredictedChurnIntEf)
table(mydata_test$Churn, mydata_test$PredictedChurn)

names(mydata)
# Data Plan X Data Usage
# Data Plan X Monthly Charge
# Monthly Charge X DayMins
# Monthly Charge X OverageMins


