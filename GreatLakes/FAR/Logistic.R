getwd()
setwd("C:/Home/Work/GreatLakes/FAR")
file = read.csv(file = "05.loan_default.csv", header = T)
install.packages("caTools")
library(caTools)

install.packages("DMwR") #Over Sampling
library(DMwR)

set.seed(100)

splits = sample.split(file$bad_flag, SplitRatio = 0.75)

Training = subset(file, splits == TRUE)
str(Training)

#SMOTEing - Oversampling
TrainingS = Training
TrainingS$bad_flag = as.factor(TrainingS$bad_flag)
str(TrainingS)
TrainingSMOTE = SMOTE(bad_flag~.,data = TrainingS, perc.over = 500)
TrainingSMOTE


#Check for Stratification
table(file$bad_flag)/nrow(file)
table(Training$bad_flag)/nrow(Training)
table(TrainingSMOTE$bad_flag)/nrow(TrainingSMOTE)

#building logistic regression
model = glm(bad_flag ~., data = TrainingSMOTE, family = binomial(link = "logit"))
summary(model)

#create the test Data and make prediction
TestData = subset(file, splits == FALSE)
PredictTest = predict(model, newdata = TestData, type = "response")
head(PredictTest,10)


#Use the naive decioin boundry p -0.5
predictClass = PredictTest > 0.5

#use the table function for the confusion matrix
confusion_matrix = table(TestData$bad_flag,predictClass)
confusion_matrix

