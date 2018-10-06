#Date 15-Jun-2017
#This is a basic scriots using Tire data for learinng basics of Conjoint analysis.
# I have already analysed COnjoint using Excel.
# There I have used MLR for OLS (Ordinary Least Square) and Used SOlver to do LAD (Least Absolute Deviation)
# Thanks to PKV sir.

# Will start with 1 user rating
# MLR with Levels
# MLR with Effect Coding
# (Constrain is MLR uses Least Square function to derive coefeciant)
library(dplyr)
# Will use Level coding.
library(conjoint)
library(googleVis)
getwd()
setwd("C:/Home/Work/Data Science/models-and-methods")
DataWith_1_Rating = read.table("Conjoint/Data/Tire_Raw.csv", header = T, sep = ",")
gvt = gvisTable(DataWith_1_Rating)
plot(gvt)

#In excel We used Dummy Effect Coding for pefroming Conjoint Analysis. 
# Dummy effect coding converts the categorical data to a integer data in the combination of 1,0 and -1,in an orthoganal way.
# Here, we will convert the categorical variable to Factors and run conjoint.

sapply(DataWith_1_Rating, FUN = "summary")
str(DataWith_1_Rating)
#Converting The Integers to String Factors
str(DataWith_1_Rating$Tread.Mile)
f_s_TreadMile = as.character(DataWith_1_Rating$Tread.Mile)
f_s_TreadMile = factor(f_s_TreadMile, levels = c("30000","40000","50000"))
f_s_Price = as.character(DataWith_1_Rating$Price)
f_s_Price = factor(f_s_Price, levels = c("50","60","70"))
DataWith_1_Rating = cbind(DataWith_1_Rating,f_s_TreadMile,f_s_Price)
gvt = gvisTable(DataWith_1_Rating)
plot(gvt)

#Converting The Integers to Numeric Factors
DataWith_1_Rating$Tread.Mile = factor(DataWith_1_Rating$Tread.Mile, levels = c(30000,40000,50000), ordered = TRUE)
DataWith_1_Rating$Price = factor(DataWith_1_Rating$Price, levels = c(50,60,70), ordered = TRUE)
str(DataWith_1_Rating)
#executing above we will knw that every variable is now converted to a factour and we can run lm

#Running linear regression to get he Attribute Siginicance
lR_data_based_on_integer_factors= lm(Response~Brand+Tread.Mile+Price+Sidewall, data = DataWith_1_Rating)
lR_data_based_on_char_factors= lm(Response~Brand+f_s_TreadMile+f_s_Price+Sidewall, data = DataWith_1_Rating)

summary(lR_data_based_on_integer_factors)
summary(lR_data_based_on_char_factors)


## Let us apply Conjoint and study the Significance
Conjoint_Parameter_Estimators_IntFactors = caModel(y = DataWith_1_Rating$Response, x = DataWith_1_Rating[,c("Brand","Tread.Mile","Price","Sidewall")])
Conjoint_Parameter_Estimators_ChrFactors = caModel(y = DataWith_1_Rating$Response, x = DataWith_1_Rating[,c("Brand","f_s_TreadMile","f_s_Price","Sidewall")])
Conjoint_Parameter_Estimators_ChrFactors

#Now let us run the conjoint
utils = c("Goodrich","Goodyear","Sears","30000","40000","50000","50","60","70", "Black","White")
utils.df = as.data.frame(utils)
Conjoint_IntFactors = Conjoint(y = DataWith_1_Rating$Response, x = DataWith_1_Rating[,c("Brand","Tread.Mile","Price","Sidewall")], z=utils.df)
Conjoint_CharFactors = Conjoint(y = DataWith_1_Rating$Response, x = DataWith_1_Rating[,c("Brand","f_s_TreadMile","f_s_Price","Sidewall")], z=utils.df)
IntFactorsTotalUtility = caTotalUtilities(y = DataWith_1_Rating$Response, x = DataWith_1_Rating[,c("Brand","Tread.Mile","Price","Sidewall")])
IntFactorsTotalUtility
#Based on the type of the data we choose when runnig the Conjoint, Results differ. But, If int is kept as integers, results are accurate with Manual conjoint

#Let now we are running the conjoint for only one user rating. It is assumed that these ratings are average of all the rating.
#Now let us take a real life scenario of multiple user rating and measure the relative attrinute importance and relatrive utility imprtance
AttributeData = DataWith_1_Rating
RatingData = read.table("Conjoint/Data/Tire_Multiple_Ratings.csv", sep = ",", header = T)
#Let us run the Estimators
MultiY_ParamEst_IntFactors = caModel(RatingData[33,2:19],x = DataWith_1_Rating[,c("Brand","Tread.Mile","Price","Sidewall")])
MultiY_ParamEst_IntFactors

#let us run the Conjoint Study
MultiY_Conjoint = Conjoint(y=RatingData[,2:19],x = DataWith_1_Rating[,c("Brand","Tread.Mile","Price","Sidewall")], z=utils.df)
