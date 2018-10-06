## My First R Project
# Date 29-Mar-2017
# day of Yugadhi

#Lets Start with Basic Math

#Arithmetic Operations
1 + 1
1+2+3
3*7*2
4/2
4/3
#PEMDAS
4*6+5
(4*6)+5
4*(6+5)

#Variables
#chapter - 4

## Variables
#Assignments
x<-2
x

y=5
y
#arrow in other direction
3 -> zz
a <- b <- 4
a
b

#using assign function
assign("j",3)
j

#Removing Variables
rm(a)
rm(b,j,x,y,z)

## Data Types
##Character data type

Char <- "data"
Char

CharAsFactor <- factor("data")
CharAsFactor

#length 
nchar(56)
nchar(Char)
nchar(CharAsFactor) # <-- This will throw error

##Dates
#Date Object(days) and POSIXct Object(seconds)
#Date Object
date1 <- as.Date("2017-03-29")
date1

class(date1)
as.numeric(date1)

#POSIXct
date2 <- as.POSIXct("2017-03-29 19:18")
date2class(date2)
as.numeric(date2)
as.numeric(date1) == floor(as.numeric(date2)/60/60/24)

## We can do more with dates using following libraries# lubridate# chron
##--------------------------------------
##install.packages(c("lubridate", "chron"))

## Logical 
# TRUE / 1
# FALSE / 0
# TRUE * 5 = 5
# FALSE * 5 = 0 
TRUE * 5 
FALSE * 5
l <- TRUElclass(l)
is.logical(l)
2==3
2 != 3
is.logical(2!=3)

## Vectors
# Collection of elements all of the same type# 
c(2,3,4,567)
c("santhosh","murali","kamakshi","Nivasini","Shashwathi")
vectorX <- c(1,2,3,4,5,6,7,8,9)vectorX

#Vector Operations
vectorX <- vectorX*3
vectorX
vectorX <- vectorX + 4
vectorXvectorX <- sqrt(vectorX)
vectorX
# operator ":" can be used to create sequence of numbers forming a vector
1:51:1000
1:200/3
sqrt(1:1000)
vectorX <- sqrt(1:1000)
vectorX
vectorX <- -100:100
vectorX
#Arithmetic Operations of 2 vectors of same size
vectorX<-1:100
vectorY<--100:-1
vectorY + vectorX
vectorY - vectorX
vectorY / vectorX
vectorY * vectorX
vectorY ^ vectorX
length(vectorY)

