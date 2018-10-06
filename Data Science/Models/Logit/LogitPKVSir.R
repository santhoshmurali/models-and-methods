#Logistic Regression PKV Sir approach
#Required Libraries 
library(googleVis)
library(corrgram)
library(Deducer) #ROC curve
library(sqldf)
library(lmtest) #log likelihood
library(pscl) #Pseudo r squared

#Read the Data from CSV File
getwd()
setwd("C:/Home/Work/Data Science/models-and-methods")
mydata = read.table(file = "Logit/Data/CellPhone.csv", header = T, sep = ",")
names(mydata)[1] = "Churn"
sapply(mydata, function(x) summary(x))

#Check for Correlation 
corData  = cor(mydata[,-1],method = "pearson")
corData
corrgram(mydata[,-1], order = TRUE, lower.panel = panel.pie, upper.panel = panel.pie, text.panel = panel.txt, main="Correlation Matirx", diag.panel = panel.minmax)
#Highly correlated variables
# Data Plan X Data Usage
# Data Plan X Monthly Charge
# Monthly Charge X DayMins
# Monthly Charge X OverageMins

#Performing Linear Regression
regression = lm(Churn~., data = mydata)
summary(regression)
lrPredict = predict(regression)
df_lrPredict = as.data.frame(lrPredict)
sqldf('select count(1) from df_lrPredict where lrPredict>1 or lrPredict <0 ')
#There are 474 observations, whose probability is beyond the realistic range of 0 to 1. Hence, we are using Logistic regression, instead of Linear regression.

#Predicting the Churn using Logistic Regression
#Splitting the data into 70:30
# 70% for Training
# 30% for Testing
set.seed(45)
splitPropotion = sample(nrow(mydata),nrow(mydata)*.7)
mydata_train = mydata[splitPropotion,]
mydata_test = mydata[-splitPropotion,]
#As we know there are 4 variables that are highly correlated in the following order, we will use them as interaction effect while defining the equation.
#Highly correlated variables
# Data Plan X Data Usage
# Data Plan X Monthly Charge
# Monthly Charge X DayMins
# Monthly Charge X OverageMins


#Model Equation and Model
logitModel = glm(Churn~AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins+DataPlan*DataUsage+DataPlan*MonthlyCharge+MonthlyCharge*DayMins+MonthlyCharge*OverageFee,data = mydata_train, family = binomial)

#Testing for Goodness of fit
### Log likelihood Ratio Test
lrtest(logitModel)
## The overall test of the model is significant based on the Chisq test and it is highly significant. This tells that likelihood of any customer churning out is heavily dependent on all the variables or at least one variable.

###Pseudo R Square
pR2(logitModel)
## McFadden RSquare is 30%, which means that 30% of uncertainty of intercept only model (model2 in likelihood ration test) has been explained by full model (model1).
## goodness of fit is reasonable

#Summary of the model
summary(logitModel)
# Customers who has recently renewed the contract,  with significant usage of data during the day, customers’ whose monthly charges are more, and who has paid significantly large amount in past 12 months is negatively impacting the churn, which means they are churning out. Especially recent contact renewals.  
# Customers having data services and also users of roaming services are positively impacting the churn. Especially customers with Data as they are Statistically Significant. 
# Customers who frequently call the call centers seems to be well informed and feel more loyal, which is what reflecting in Positive impact with high Significance. 
# all the correlated variables are statistically very significant and also most of them are positive.
# Deviance has dropped sharply from Null to Residual, that signifies that Independent variables are impacting the churn strongly
# Company has to look in the existing contact renewal model to control churn, and also revisit and improvise its data plan so that we can bring in more customers to data and control churn.
# There is a strong correlation between Data Plan and Usage and Charges, also, significantly impacting the churn. Company might want to look into pricing structure as well.


#Odds and Probability
odds = exp(coef(logitModel))
prob = odds/(odds + 1)
odds_and_prob = cbind(as.data.frame(odds),as.data.frame(prob)[,1])
names(odds_and_prob)[2] = "prob"
odds_and_prob
# If data plan changes by 1 unit, the odds for churn is impacted by 8616 times, compared to the loyal customers.
# Similar trend we can see for DataPlan with DataUsage
confint(logitModel)
# Confident interval also tells the same story


# Lets us now Predict the Test data using the model
logitPredcit = predict(logitModel,newdata = mydata_test, type = "response")
df_logitPredcit = as.data.frame(logitPredcit)
sqldf('select * from df_logitPredcit where logitPredcit > 1 or logitPredcit < 0')
# Since we have converted them to Probability based on Exponentials, we do not see negative probability. This way we have brought all the likelihood with the realistic range.

# Setting the cut-off value.
ChurnPredicted = as.data.frame(ifelse(logitPredcit>.5,1,0))
names(ChurnPredicted)[1] = "ChurnPredicted"
# Testing the Performance or Accuraracy of Fit
## Confusion matrix or classification table
glm_CM=table(mydata_test$Churn, ChurnPredicted$ChurnPredicted)
glm_CM = as.matrix(glm_CM)
glm_TP = glm_CM[1,1]
glm_FN = glm_CM[1,2]
glm_FP = glm_CM[2,1]
glm_TN = glm_CM[2,2]

# accuracy = (TP+TN)/(TP+FN+FP+TN)
glm_accuracy  = (glm_TP + glm_TN)/(glm_TP+glm_TN+glm_FP+glm_FN)
glm_accuracy
# model is 88% Accurate

# Specificity = TN/(TN+FP)
glm_Specificity = glm_TN/(glm_TN+glm_FP)
glm_Specificity
# 35% of time model predicted churning customers correctly.

# Sensitivity =TP/(TP+FN)
glm_Sensitivity = glm_TP/(glm_TP+glm_FN)
glm_Sensitivity
# 98% of times model predicted non churning customers correctly.

##ROC Curve: Receiver Operating Characteristic(ROC) 
# model’s performance by evaluating the trade offs between true positive rate (sensitivity) and false positive rate(1- specificity). 
# with assumption of cut of point at .5
#.90-1 = excellent (A)
#.80-.90 = good (B)
#.70-.80 = fair (C)
#.60-.70 = poor (D)
#.50-.60 = fail (F)
rocplot(logitModel)
#Here we have AUC - 0.86, which is a good predictive model

# Conclusion
# using the equation "AccountWeeks+ContractRenewal+DataPlan+DataUsage+CustServCalls+DayMins+DayCalls+MonthlyCharge+OverageFee+RoamMins+DataPlan*DataUsage+DataPlan*MonthlyCharge+MonthlyCharge*DayMins+MonthlyCharge*OverageFee
# we should be able to predict the churning customer 86% of time accurately.
# Organization must look into Data Plan , Usage and Charges to control the churn.



