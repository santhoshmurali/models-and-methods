#Required Libraris
#library(didrooRFM)
#library(plyr)

getwd()
setwd(dir = "c:/___/")
casino <- read.csv("Casino.csv", stringsAsFactors = F)
hotel <- read.csv("Hotel.csv", stringsAsFactors = F)

casino_final <- casino[,c(1,4,17)]

names(casino_final)[1] <- 'CustomerID'
names(casino_final)[2] <- 'DateofPurch'
names(casino_final)[3] <- 'Amount'
casino_final$DateofPurch <- as.Date(casino_final$DateofPurch,"%d-%b-%y")

hotel_final <- hotel[,c(2,4,8)]
names(hotel_final)[1] <- 'CustomerID'
names(hotel_final)[2] <- 'DateofPurch'
names(hotel_final)[3] <- 'Amount'
hotel_final$DateofPurch <- as.Date(hotel_final$DateofPurch,"%d-%b-%Y")

mgm <- rbind(casino_final,hotel_final)
mgm$TransNo <- 1:nrow(mgm)
mgm_final <- subset(mgm, select=c(4,1:3))

library(didrooRFM)
library(plyr)

RFM <- findRFM(mgm_final, recencyWeight = 5, frequencyWeight = 5, monetoryWeight = 5)
table(RFM$FinalCustomerClass)

cas_temp <- ddply(casino, .(PLAYER_ID), summarize, Casino_Earnings = sum(TOTALEARNINGS))
names(cas_temp)[1] <- "CustomerID"
hotel_temp <- ddply(hotel, .(PLAYER_ID), summarize, hotel_Earnings = sum(Earnings_TTL))
names(hotel_temp)[1] <- "CustomerID"

RFM <- merge(RFM, cas_temp, by = 'CustomerID', all.x = T)
RFM <- merge(RFM, hotel_temp, by = 'CustomerID', all.x = T)

RFM$Casino_Earnings[is.na(RFM$Casino_Earnings)] <- 0
RFM$hotel_Earnings[is.na(RFM$hotel_Earnings)] <- 0

RFM$Mgm_Earnings <- RFM$MeanValue * RFM$NoTransaction

write.csv(RFM, "RFM_Output.csv", row.names = F)

Contribution <- ddply(RFM, .(FinalCustomerClass), summarize, Cas_Earning = sum(Casino_Earnings),  Hotel_Earning = sum(hotel_Earnings), Casino_Cont = sum(Casino_Earnings)/abs(sum(Mgm_Earnings)), Hotel_Cont = sum(hotel_Earnings)/abs(sum(Mgm_Earnings)))

write.csv(Contribution, "Contribution.csv", row.names = F)
