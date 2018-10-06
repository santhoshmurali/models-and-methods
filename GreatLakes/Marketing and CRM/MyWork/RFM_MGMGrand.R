getwd()
setwd(dir = "C:/Home/Work/GreatLakes/Marketing and CRM")

any(grepl("rJava", installed.packages()))

install.packages("XLConnect")
#To avoid Heapsize error
library(rJava)
#Ref : https://stackoverflow.com/questions/27153974/how-to-fix-outofmemoryerror-java-gc-overhead-limit-exceeded-in-r
options(java.parameters = "-Xmx8000m")
library(XLConnect)


dfXL =  readWorksheetFromFile(file ="Data Set - MGM Grand.xlsx", sheet = 2)
MGMCasinoSheet = dfXL

#Profiling the data
MGMCasinoSheet[1:100,c(1,2,4,19)]
#Moving the data required for RFM to another object
MGMCasinoRFMData = MGMCasinoSheet[,c(1,2,4,19)]
str(MGMCasinoRFMData)
#This is required to find recency.
TheMostRecentDate = max(MGMCasinoRFMData$MAXDATE)
names(MGMCasinoRFMData)[4] = "Earnings"
names(MGMCasinoRFMData)[3] = "VisitDate"
head(MGMCasinoRFMData)
#orderig the d based on date
MGMCasinoRFMData = MGMCasinoRFMData[order(MGMCasinoRFMData[,"VisitDate"],decreasing = T),]

f_SITEID = factor(MGMCasinoRFMData$SITEID, labels = c("The Mirage", "Treasure Island", "MGM Grand Las Vegas", "New York New York", "MGM Grand Detroit", "Bellagio", "Beau Rivage"))
MGMCasinoRFMData = cbind(MGMCasinoRFMData,f_SITEID)


#