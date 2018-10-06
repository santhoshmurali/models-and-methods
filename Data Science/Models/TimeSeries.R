mcx_data = read.csv(file = "C:\\Home\\Work\\Ordinarily\\ML_Trading\\MCX_Daily_Data.csv", header = TRUE, sep = "|")
View(mcx_data)

mcx_nickel_current=mcx_data[(mcx_data[,8]=='Nickel' & mcx_data[,9]=='Current'),]

mcx_nickel_current_open = mcx_nickel_current[,c(1,2)]

library(xts)

mcx_nickel_current_open_ts = xts(mcx_nickel_current_open[,-1],order.by = as.Date(mcx_nickel_current_open$Date,"%Y-%m-%d"))


mcx_nickel_current_open_ts

View(diff(mcx_nickel_current_open_ts,arithmetic = TRUE))

rollmean(mcx_nickel_current_open_ts,k = 6)
par(mfrow=c(1,1))

mcx_nickel_current_open_acf= acf(mcx_nickel_current_open_ts,lag.max = 2, type = 'correlation',plot = TRUE)

write.csv(mcx_nickel_current_open,file = "C:\\Home\\Work\\Ordinarily\\ML_Trading\\MCX_Daily_Data_nickel_current.csv") 

mcx_nickel_current_open_acf


