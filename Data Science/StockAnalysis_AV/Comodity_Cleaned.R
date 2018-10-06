# Comparative Stock Market Analysis in R using Quandl & tidyverse- Part I
## Introduction

#https://www.analyticsvidhya.com/blog/2017/09/comparative-stock-analysis/

#What differentiates the best data scientists from others? It is their focus on application of data science. The best data scientists I know of, see data science and its application every where they look. They look at this world as an outcome of flow of data and information.

#On the other hand, most beginners often ask the question – how do we apply our learning on real life problems?

#In this post (and another one following this), I have picked up a real life dataset (Stock Markets in India) and showed how I would use this data to come out with useful insights.

#I hope that you will find this useful. The idea is show the vast opportunities present in data science in a simple yet powerful manner. If you can think of more examples like this – let me know in comments below!
  
#  For the best results, I would strongly recommend to build the application yourself as you follow the tutorial.

# Objective of this Tutorial
#In this article, we will analyze Commoditiy Market in metals segment based on the Lead that is  listed in MCX India. Our objective is to find the trends (Seasonal or cyclic) in it.

#In our comparative analysis we will use several packages and the primary focus will be on tidy verse package. The emphasis will be given on grouping with the help of **tibble** dataframe from tidy verse package. This will help to perform similar operation on multiple groups at a time, hence reducing the code length and computational time

#This article also focuses on API Key, database code search using quandl, and finally how to directly download the data from R Console
#So lets get started!
#Note: The code that has been mentioned below is to be run on the R command line for best results.

#Table of Contents
#* Setting up the system
#* Getting Started with Comparative Analysis
#* * Creating the dataset
#* * Visualizing the monthly prices
#* * Discovering the Relation between Total Traded Quantity vs Close Price
#* * Finding the Density Distribution of Deviation of High Price from Open Price
#* * Observing the Autocorrelation lags

#### Setting Up The System
#There are a few things you should take care of before you go on further. Below mentioned are the packages you need to install in the system
#- Quandl for Data Download
#- timetk to coerce the dataframe into xts
#- tidyverse to Use tibble for grouping and perform single operation on multiple groups
#- tidyquant for Time Series and Financial functions to perform the analysis
#- gglot for Plotting and Visualization
#- gganimate to plot the monthly prices. To get more information on gganimate, please read my previous post on Analytics Vidhya
#- forcats for modification of factor levels
#- stringr for string use

library(plyr)
library(zoo)
library(Quandl)
library(tidyverse)
library(timetk)
library(forcats)
library(stringr)
library(gganimate)
library(dplyr)
library(stringr)
library(gridExtra)
library(plotly)
library(ggplot2)

#-----------------------------------------------------------------------------------------
#Creating the Dataset
#We will be using Quandl is online repository for the core financial, macroeconomic statistics and forex. Quandl has a vast collection of free and open data collected from a variety of organizations: central banks, governments, multinational organizations and more. You can use it without payment and with few restrictions.

#Both Free and Premium data are available. Authenticated Free users have a limit of 300 calls per 10 seconds, 2,000 calls per 10 minutes and a limit of 50,000 calls per day. Premium data subscribers have a limit of 5,000 calls per 10 minutes and a limit of 720,000 calls per day.

#We will use this online repository to get our data using “Quandl” package directly from the R Console. Quandl package directly interacts with the Quandl API to offer data in a number of formats usable in R, downloading a zip with all data from a Quandl database, and the ability to search.

#For More information on Quandl Package, please visit this page. https://cran.r-project.org/web/packages/Quandl/Quandl.pdf

#To get started with Quandl, create an account and get the quandl API key. Please click here to create an account. Then click on the Login button provided on the top right corner of the screen. Once the registration is complete, please click here to get the API Key.

#Login : https://www.quandl.com/ API Key : https://www.quandl.com/account/api

#unpw: my gmail uspw
#-----------------------------------------------------------------------------------------



## Setup the Quandl Free Account and API Key, Please copy and paste the API key in order to #authenticate
Quandl.api_key("rMrxniUJH1-pmm4edrqA")

#### We will use MCX database with Lead commodity for analysis
# www.quandl.com/data/MCX-Multi-Commodity-Exchange-India

#You see there are Multiple data sets we have for Lead. Every contact is a seperate Data set. 
#for  example
#- Lead Futures, October 2014, **PBV2014**, MCX
#- Lead Futures, February 2016, **PBG2016**, MCX

#We will have to get all those individual data sets seperatly and merge them as one set 

# ** Serach for Lead for last 36 contacts or 3 years of data**

#-----------------------------------------------------------#
################ Actual Program Begins Here #################
#-----------------------------------------------------------#
#ContractStart Date and End date
StartDate=as.Date("2017-07-01", format = "%Y-%m-%d")
EndDate = as.Date("2018-07-01", format = "%Y-%m-%d")


MCX_Search_Results = Quandl.search("Lead Futures",per_page = 50,silent = TRUE)
MCXFuturesMeta_DF = data.frame(Name = MCX_Search_Results$name, Exchange = MCX_Search_Results$database_code,DataSetCode = MCX_Search_Results$dataset_code,ContractDate=as.Date(str_c('01 ',str_replace(word(MCX_Search_Results$name,3,4),",","")),format = '%d %B %Y'),ContractExpiry=str_replace(word(MCX_Search_Results$name,3,4),",",""))
MCXFuturesMeta_DF["ExpiryMonth"]=word(MCXFuturesMeta_DF$ContractExpiry,1)
MCXFuturesMeta_DF["ExpiryYear"]=word(MCXFuturesMeta_DF$ContractExpiry,2)
MCXFuturesMeta_DF["Code_Abbrv"] = str_sub(MCXFuturesMeta_DF$DataSetCode,1,3)
MCXFuturesMeta_DF = MCXFuturesMeta_DF[order(MCXFuturesMeta_DF$ContractDate),]


#Code_Month_Mapping
MCX_Future_Code_Month_Mapping = unique(MCXFuturesMeta_DF[,c("ExpiryMonth","ExpiryYear","Code_Abbrv")])
MCX_Future_Code_Month_Mapping["Key"]=1
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='February',"Key"]=2
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='March',"Key"]=3
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='April',"Key"]=4
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='May',"Key"]=5
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='June',"Key"]=6
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='July',"Key"]=7
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='August',"Key"]=8
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='September',"Key"]=9
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='October',"Key"]=10
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='November',"Key"]=11
MCX_Future_Code_Month_Mapping[MCX_Future_Code_Month_Mapping$ExpiryMonth=='December',"Key"]=12



################## Trail Algorithem ##################

# This is the table that has mapping of code and the month
MCX_Future_Code_Month_Mapping = MCX_Future_Code_Month_Mapping[order(MCX_Future_Code_Month_Mapping$Key),]

d_code = ""
d_contract = ""
i = 1
for (k in StartDate:EndDate){
  l_code = MCX_Future_Code_Month_Mapping[as.numeric(format(as.Date(k),"%m")),2]
  l_year = format(as.Date(k),"%Y")
  l_finalCode = str_c("MCX/",l_code,l_year)
  d_code[i] = l_finalCode
  d_contract[i] = format(as.Date(k),"%B-%Y")
  i = i + 1
}

d_code = unique(d_code)
d_contract = unique(d_contract)

# This is the contract Data set
df_input_contacts = data.frame(d_code,d_contract)


# Trading Data
t = 1
temp0 = ""
temp1 = Quandl("MCX/PBH2018",type = "raw",collapse = "daily")
temp1["ContractMonth"] = max(d_contract)
temp1["ConractRank"] = 0
temp1 = temp1[-c(1:nrow(temp1)),]
for (contract in df_input_contacts$d_code)
{
 temp0 = Quandl(contract, type = "raw", collapse = "daily")
 temp0["ContractMonth"] = df_input_contacts$d_contract[t]
 temp0["ConractRank"] = t
 temp1 = rbind(temp0,temp1) 
 t = t + 1
}

MCX_Master_Data = temp1

Mean_Metrics = MCX_Master_Data %>% group_by(ConractRank,ContractMonth) %>% select(Volume,Open, High, Low, Close) %>% summarise(
  Vol = round(mean(Volume, na.rm = T),2),
  Clo = round(mean(Close, na.rm = T),2),
  lw = round(mean(Low, na.rm = T),2),
  hi = round(mean(High, na.rm = T),2),
  opn = round(mean(Open, na.rm = T),2)
)

Mean_Metrics = as.data.frame(Mean_Metrics)


## Exploratory Analysis


plt_mean_metrics_1 = ggplot(data=Mean_Metrics,aes(x=ContractMonth, y=Clo))+
  geom_bar(stat = 'identity', width = 0.5) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  geom_text(aes(label = Clo), vjust=-0.3, size=2.5) + theme(legend.position = "top")

plt_mean_metrics_1 

Mean_Metrics
