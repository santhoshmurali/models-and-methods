## Statistical Models and Decision Making
## Instructor : P.K. Vishwanathan
## Date : 19-Jan-2017
#CASE 1

HealthCare = read.csv("C:/Home/Work/GreatLakes/Statistical Methods and Decision Making - PKV/Health.csv")
summary(HealthCare)
#Queston 1
# Work score given by nurses substantially more than Pay and Promotions, in all measurre of central tendancy.
# For Example:
#       Work            Pay          Promotion    
#Median :82.00   Median :55.50   Median :58.50  
#Mean   :79.80   Mean   :54.46   Mean   :58.48  

sd(HealthCare$Work)
HealthCare$WorkSD = sd(HealthCare$Work)
HealthCare$PaySD = sd(HealthCare$Pay)
HealthCare$PromotionSD = sd(HealthCare$Promotion)

HealthCare$Workcv = HealthCare$WorkSD/mean(HealthCare$Work)
HealthCare$Paycv = HealthCare$PaySD/mean(HealthCare$Pay)
HealthCare$Promotioncv = HealthCare$PromotionSD/mean(HealthCare$Promotion)

HealthCare$WorkRange = range(HealthCare$Work)
HealthCare$PayRange = range(HealthCare$Pay)
HealthCare$PromotionRange = range(HealthCare$Promotion)


summary(HealthCare)

HealthCareModified <- HealthCare

attach(HealthCare) ## attaches Healthcare to buffer so the columns are directly accessabe without $

## Function for  finding mode _________________________________________
Freequency = table(as.vector(Work))
Mode = names(Freequency)[Freequency == max(Freequency)]
Mode
##______________________________________________________________________

# take only Hospital, Work, Pay, Promotion
MyHospitalData = HealthCare[,2:5]
attach(MyHospitalData)
MyHospitalData

# Returns Aggrigate Representation of the table in Hospita wise
SummaryHospitalWise = by(MyHospitalData, INDICES = Hospital, FUN=summary)
SummaryHospitalWise
Mean = c(mean(Work), mean(Pay), mean(Promotion))
Mean
SD = c(sd(Work), sd(Pay), sd(Promotion)) #Standard Deviation
SD
CV = SD/Mean #Coefficiant Variable
CV
VariationSummary <- data.frame(Mean,SD,CV)
VariationSummary
row.names(VariationSummary) = c("Work","Pay","Promotion")

as.array(SummaryHospitalWise)

## Class Assignments
## To Build histogram without using hist function

Work
table(Work)
RangeHealthWork = range(Work)
BinsHealth = round(RangeHealthWorkDiffernce/sqrt(length(Work)))
StartValue = ifelse(RangeHealthWork[1]%%5!=0,RangeHealthWork[1]-RangeHealthWork[1]%%5,RangeHealthWork[1]-1)
RangeHealthWorkDiffernce = RangeHealthWork[2] - RangeHealthWork[1]
BinsHealth = round(RangeHealthWorkDiffernce/sqrt(length(Work)))
xAxisFreequency = seq(from=StartValue, to=RangeHealthWork[2], by=BinsHealth)


table(cut(Work,xAxisFreequency)) #Cut is used for aggrigating values and group it into bins
workRange = table(cut(Work,xAxisFreequency))
names(workRange) = xAxisFreequency[-1]
workRange
barplot(workRange, space=0, xlab="Work", ylab="Freequency")


#Same Freequecy distribution without usinh his for "Pay"
# creates a seq with bins
slab=seq(from=round(min(Pay)-((max(Pay)-min(Pay))/(sqrt(length(Pay)))),0), to = round(max(Pay)+((max(Pay)-min(Pay))/(sqrt(length(Pay)))),2), by = ((max(Pay)-min(Pay))/(sqrt(length(Pay)))))
# Groups and aggrigates the value in the ranges
Ranges = (cut(Pay,ceiling(slab),right = FALSE))
# Counts the data and puts in corresponding bins
FreequencyDistPay = table(Ranges)
names(FreequencyDistPay) = ceiling(slab)[-1]
barplot(FreequencyDistPay, space = 0, xlab="Pay", ylab="Score", border = 0)


## Statistical Models and Decision Making
## Instructor : P.K. Vishwanathan
## Date : 20-Jan-2017
## Case 2 - CardioGood Fitness
##The market research team at AdRight is assigned the task to
#identify the profile of the typical customer for each treadmill
#product offered by CardioGood Fitness. The market research
#team decides to investigate whether there are differences across
#the product lines with respect to customer characteristics. The
#team decides to collect data on individuals who purchased a
#treadmill at a CardioGood Fitness retail store during the prior
#three months. The data are stored in the CardioGoodFitness.xls
#file. The team identifies the following customer variables to study:
#  product purchased, TM195, TM498, or TM798; gender; age, in
#years; education, in years; relationship status, single or partnered;
#annual household income ($); average number of times the
#customer plans to use the treadmill each week; average number
#of miles the customer expects to walk/run each week; and self-
#rated fitness on an 1-to- 5 ordinal scale, where 1 is poor shape
#and 5 is excellent shape.
# 1. Compute descriptive statistics to create a customer profile for
#each CardioGood Fitness treadmill product line.
#2. Write a report to be presented to the management of CardioGood
#Fitness, detailing your findings.

CardioGoodFitness.src = read.csv("C:/Home/Work/GreatLakes/R/ClassWorkOuts/CardioGoodFitness.csv")
attach(CardioGoodFitness.src)

#Building Cross table
table1 = round(prop.table(table(Product, Gender))*100,2) #Will also calculate the %
#Intepretation for Customer Profile
#Gender
#Product Female Male
#TM195     40   40
#TM498     29   31
#TM798      7   33

# Product TM195 is equaly purched by males and females, 
# TM498 is almost equal males and female while 
# TM798 is more used by Males
# TM198 is sold more and TM798 is sold less.
# Males spend more in Fitness products
table2 = round(prop.table(table(Product, MaritalStatus))*100,2) #Will also calculate the %

table3 = round(prop.table(table(Product, Gender,MaritalStatus))*100,2) #Will also calculate the %
# When peopl get married, they tend to prchase items. In that TM195 has highest rate of purcahse rate shift from single to married, esp. Female.

table3
table2

table(Product, MaritalStatus)



ageGroups = seq(from=min(Age), to = max(Age), by=8)
table(cut(Age,ageGroups,right = FALSE))
table

by(CardioGoodFitness.src, INDICES = Product, FUN = summary)
# Number of Males and Females are Equal.
# income of the group of people 798 is more than other two products
summary(CardioGoodFitness.src)

boxplot(Age~Product, horizontal = TRUE, col=rainbow(8))
# for TM195 is skewed towards 25
boxplot(Education~Product, horizontal = TRUE, col=rainbow(9))
boxplot(Income~Product, horizontal = TRUE, col=rainbow(9), xlab="Salary")
plot(Age,Product)

# Q1  = 1 * ((n+1)/4)
# Q2  = 2 * ((n+1)/4)
# Q3  = 3 * ((n+1)/4)
# If Q is decimal

boxplot(Usage~Product)
boxplot(Usage~Product)

FitnessSource = read.csv("C:/Home/Work/GreatLakes/R/ClassWorkOuts/CardioGoodFitness.csv")
attach(FitnessSource)
tbl_Prodcut_Education_gender = table(Product,Education,Gender)
plot(tbl_Prodcut_Education_gender)

## Statistical Models and Decision Making
## Instructor : P.K. Vishwanathan
## Date : 21-Jan-2017

#Binomial Distribution

#x =  possible outcome
#n = number of trials
#p = Sucess Percentage
#failure = 1-p
# formlula is nCx.p^x.(1-p)^(n-x)


binom.test(2,7,0.6)
??"binomal distribution"

dbinom(c(0:7),1,.6)
dbinom(46:54, 100, 0.5)
dbinom(0:7,1,.6)


sucess=c(0,1,2,3,4,5,6,7)
probabilities = dbinom(size=7,prob = 0.6,sucess)
CumulativeProbabilities = pbinom(size=7,prob = 0.6,sucess)
BiProbabilities = data.frame(sucess,probabilities,CumulativeProbabilities)
names(BiProbabilities)
plot(BiProbabilities)


#Poison Distribution
??"Poison Distribtion"
dpois(4,lambda = 3)
ppois(3,lambda = 3, lower=FALSE)

#Normal Distributon
??"Normal Distribution"
Mean = 0.295 #- Mean weight of Mreakfast ceral
SD = 0.025 # SD 
# Z = (x-mean)/sd
#http://www.stat.ufl.edu/~athienit/Tables/Ztable.pdf
# if x is < mean then z is negative then p
# if x is > mean then z is positive then 1-9

# x is less then 0.280
z = (0.280 - 0.295)/0.025
z = -0.6
# check in the abovr table
p = .2743

# x is greater then 0.350
z = (0.350 - 0.295)/0.025
z = 2.2
# check in the abovr table
p = 1 - .9861
# p = 1.3%

# x is between 0.260 and 0.340 
# use pnorm
z1 = (0.260 - 0.295)/0.025
z2 = (0.340 - 0.295)/0.025
p1 = 0.0808
p2 = 0.9641
p = p2 - p1
p
# what is the value I will get in normal distribution if the wight is at 90%
#use qnorm
z = .9
#x = ?
x = (z*(0.025)) + Mean
#x = 0.3175 Kgs
qnorm(0.90, Mean, 0.025, lower = FALSE)



# Theme 5
# Hypothesis Testing is Decision Oriented



## Statistical Models and Decision Making
## Instructor : P.K. Vishwanathan
## Date : 21-Jan-2017

# 11th excecise

#t-test
getwd()
GOlfData = read.csv("C:/Home/Work/GreatLakes/R/ClassWorkOuts/Golf.csv", header = TRUE)
attach(GOlfData)
tstat1 = t.test(Current, New)
tstat1
tstat2 = t.test(Current, New, var.equal = TRUE)
tstat2


# Paired
tstat3=t.test(XA,XB, paired = TRUE)


#Chi-Squiare
WinLow = c(4,8,16,10)
WinMidLow = c(17,23,20,17)
WinMidHigh = c(17,22,14,19)
WinHigh = c(5,27,20,11)

mydata = data.frame(WinLow, WinMidLow, WinMidHigh, WinHigh)
row.names(mydata) = c("MacLow","MacMidLow", "MacMidHigh", "MacHigh")
mydata

boxplot(chisq.test(mydata)$expected)

