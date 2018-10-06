# Setting Working Dir
setwd("C:/Home/Work/Statistics/Kaggle/Titanic Machine Learning from Disaster")
TraingDataTitanic <- read.csv("TrainingData.csv",sep=",")

dplyr::tbl_df(iris)

dplyr::glimpse(iris)

utils::View(iris,"Irsi")

install.packages("reshape2")
install.packages("plyr")
install.packages("knitr")

??knitr

str_to_upper("hi how are you")

x <- c("a_1","a_2","a_3")
x

library(reshape2)
vars <- colsplit(x,"_",c("char","num"))
vars
require(devtools)
install.packages("devtools")
?require

require(devtools)
