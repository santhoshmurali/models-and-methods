## Introduction to Data Aanalytics
## Instructor : Krishna Mohan
## Date : 18-Jan-2017
## ===================================
## Reading different input data types
## ===================================
## Set the working directoy to read and wrie data
# Get Working Directory
getwd()
## Set the working Directory
setwd("C:/Home/Work/GreatLakes/R/ClassWorkOuts/")
## Writing and Reading Data ============================================
## Writing the data fom Library to local
## Support Functions and Datasets for Venables and Repley's
install.packages("MASS")
library(MASS)

input.data = Cars93
write.csv(input.data,file = "Cars.csv")

## Read Data from Local Machine
input.data.local = read.csv("Cars.csv", header = TRUE)

## Read data from online source
input.data.online = read.table("http://web.pdx.edu/~gerbing/data/cars.csv", sep=",")


## --------------------------------------
## Understanding Data
## --------------------------------------
## Display the first few lines
head(input.data,10)

## Display the last few lines
tail(input.data,10)

## Structure of the data
str(input.data.local)
# Factor is like categories and will have unique values

## Summary
summary(input.data)

## List the names of the columns
names(input.data)


## ==============================================================================
## Introduction to Data Aanalytics
## Instructor : Krishna Mohan
## Date : 19-Jan-2017
## ==============================================================================
## Basics
## ==============================================================================
## --------------------------------------
## Understanding Variables
## --------------------------------------
## Assignments and Arithmetics
  x = 3
  x
## Convertion from Numeric to Integer  
  y = as.integer(x + 5)
  y
  
  class(x)
  class(y)
## Assigning Charcater Values to the Variables
## One string variable  
  z_1 =  "Sachin"
## multiple string variable    
  z_m = c("Kohli","Dhoni","Ashwin","k l Rahul", "md Shami")
  z_m

  z = z_m
#----------------  
## Vector
#----------------  
## Venctor can contain either numbers, strings or logical values but not mix of them
  v1 = c(1,2,3,4,5,6,7,8,9,10) #Numeric Vector
  class(v1)
  
  v2 = c(1*pi,2*pi,3*pi) #Computed Numeric Vector
  class(v2)
  
  v3 = c("Introduction", "To", "R") #String/Character Vector
  class(v3)
  
  v4 = c(TRUE,FALSE,TRUE,FALSE) #Logical Vector
  class(v4)
  
  v5 = c(1,pi)
  class(v5)
  
  v6 = c("Introduction", "to", 6) #If we mix Character and Numeric, it considers it as Character vector
  class(v6)
  
  v64 = c(6,TRUE,FALSE) #If we mix LOGICAL and Numeric, it considers it as numeric vector converting it True to 1 and False to 0
  class(v64)
  
  v63 = c("Logical",TRUE,FALSE)
  class(v63)
  
## Creating Sequence
  s1 = 1:5 #Creates Sequence of Numbers from 1 to 5
  s1
  
  s2 = seq(from=1, to = 15, by=2) #Create Sequence of odd numbers
  s2
  
  s3 = seq(from=1, to = 15, length.out = 5) #split 1 to 15 by 5 parts
  s3
  
  s4 = rep(1:4,times = 5) #repeat 1,2,3,4 by 5 times
  s4
  
  s0_1 = seq.Date(from=as.Date('2000-01-01'), to=as.Date('2000-01-30'), by=2)
  s0_1
  
## Computing Vectors
  v.bus = c(1,1,4,5,3,2)
  v.passengers = c(4,1,5,5,1,2)
  
  v.totalPessangers = v.bus * v.passengers #Multiplying Vectors
  v.totalPessangers
  
  v.a = v.bus
  v.b = v.passengers
  v.c = v.a + v.b #Adding Vectors
  
  mean(v.a) #Mean of Vectors
  
## Comparing Vectors  
  v.bus == v.passengers
  
## Selecting Specfic or sub range with-in Range
  v.bus[3] # Display the Third Value
  v.bus[1:3] # Display first 3
  v.bus[c(1,3,5)] #Dispay only 1st, 3rd and 5th
  v.bus[-3] #Display all values except value in 3rd position

#----------------  
## Lists
#----------------  
## Can contain mixture of different types of Data Sets
##

l1 = list(2,"m",z) 
l1
class(l1)

l1[3] # Display only 3rd value

class(l1[[3]]) # Dsilayes the type of the item in 3rd position

l1[[3]][1] # Displays first value in 3 item which is a charcter vector

l1

## --------------------------------------
## Packages
## --------------------------------------
library() #Dsiplays list of packages

search() # list of packages currently loaded

install.packages("abc.data") # Install Packages

library(abc.data) #Load Packages

update.packages("abc.data") #update package

detach("package:abc.data") # Detach Package

data() # Dsiplays the available and default data sets

head(iris)


## --------------------------------------
## Getting Help 
## --------------------------------------

?mean

example("subset")

args(sd) #Displayes Argument for the function

install.packages("dplyr")

help(package = dplyr) #Displays documentation for Dplyr
??"dplyr"

## --------------------------------------
## Data Frames 
## --------------------------------------
# Data frame is nothing but a tabe

dataframe.1 = data.frame(v.a,v.b,v3) # v3 has only 3 values, but we have 6 rows. 3 values repeated itself twice
dataframe.1
str(dataframe.1)
dataframe.1.1 = data.frame(v.a,v3,z_m) #this will not work as length of z_m string vector is greater thatn v.a numeric vector. 

# Adding a new row to dataframe
new.row =data.frame(v.a = 9,v.b=9,v3="New Row added")
dataframe.2 = rbind.data.frame(dataframe.1,new.row)
dataframe.2
str(dataframe.2)

# Adding a new column to data frame
new.colum = c(1,2,3,4,5,6,7)
dataframe.3 = cbind.data.frame(dataframe.2,new.colum)
dataframe.3

# Renaming the colums in DataFame
colnames(dataframe.3)
colnames(dataframe.3) = c("bus","passengers","subject","freequency")
dataframe.3

dataframe.3$crowd = dataframe.3$bus * dataframe.3$passengers

dataframe.3_1 = dataframe.3

dataframe.3_1$crowdPercentage = dataframe.3$passengers/

dim(dataframe.3) # rows X col

dataframe.3[3,] # returns 3rd row

dataframe.3[,3] # returns all rows for 3rd col

dataframe.3[2,3] #returns data point in 2nd row and 3rd columns

dataframe.3$freequency # returns all values in freequecy column

# Data frame sorting
dataframe.3[order(dataframe.3$passengers),] #Sorting dataframe based on passenges in asecending
dataframe.3[order(dataframe.3$passengers,decreasing = TRUE),] #Sorting dataframe based on passenges in decending


## --------------------------------------
## Graphs 
## --------------------------------------

#
# x-axis        Height of bar           graph
# Continous     count                   Histogram
# Discrete      count                   Bar
# Continous     Value                   Bar
# Discrete      value                   Bar

hist(input.data.local$Horsepower, main = "Historgram of Horsepower", xlab="Horsepower",col=rainbow(5)) #Continous and count

#Colored based on Condition
hist(input.data.local$Horsepower, main = "Historgram of Horsepower", xlab="Horsepower",col=rainbow(5))
price.colors = ifelse(input.data$Price > 25,"blue","red")
barplot(input.data$Price, col = price.colors)

# 2 numeric variables with scatter plot
plot(input.data$Horsepower, input.data$Min.Price, col=price.colors)

# ggplot
## Tryout something on ggplot
install.packages("ggplot2")
library(ggplot2)

# plotly
## Tryout something on plotly < depends on ggplot
install.packages("plotly")
library(plotly)

# Volcano
volcano
plot_ly(z = ~volcano, type = "surface")
plot_ly(z = ~volcano)
