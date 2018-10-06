
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from matplotlib import pyplot
import datetime
import copy as cp

get_ipython().magic('matplotlib inline')


# #salesData = pd.read_csv("C:\Home\Work\GreatLakes\capstone\withOldData\DataMySql.csv")
# 

# In[2]:


salesData = pd.read_csv("C:\Home\Work\GreatLakes\capstone\withOldData\LatestDataFromKlachaks.csv")


# In[3]:


RawData = salesData.loc[:,['Customer_Name','End','Rentals']]

WorkingData = cp.deepcopy(RawData)

WorkingData.Rental = WorkingData.Rentals.astype('int32')

WorkingData.End = pd.to_datetime(WorkingData.End, format='%Y-%m-%d')

WorkingData.columns = ['Name','End','Rental']


# In[4]:


## FreeQuency
WorkingDataFreeq = cp.deepcopy(WorkingData)


# In[5]:


#Freequenct of visit
WorkingDataFreeq = WorkingDataFreeq.loc[:,['Name','End']].groupby('Name').count()
 
WorkingDataFreeq = WorkingDataFreeq.reset_index()
 
WorkingDataFreeq.columns = ['Name','Freeq']

#FREEQUENCY VALUE
# 5 - ANYTINGS GREATER THAN AND EQUAL TO 5
# 4 - ANYTHING GREATER THAN AND EQUAL TO 4 AD LESS THAN 5
# 3 - ANYTHING GREATER THAN AND EQUAL TO 3 AD LESS THAN 4
# 2 - ANYTHING GREATER THAN AND EQUAL TO 2 AD LESS THAN 3
# 1 - ANYTHING GREATER THAN AND EQUAL TO 1 AD LESS THAN 2
 
R5 = range(20,100000)
R4 = range(15,20)
R3 = range(8,15)
R2 = range(3,8)
R1 = range(1,3)
 
def udf_freeqIndex(freeq):
    freeqIndex = 1
    if freeq in R5:
        freeqIndex = 5
    elif freeq in R4:
        freeqIndex = 4
    elif freeq in R3:
        freeqIndex = 3
    elif freeq in R2:
        freeqIndex = 2
    else:
        freeqIndex = 1
    return(freeqIndex)


c = pd.Series(list(map(lambda x: udf_freeqIndex(x), WorkingDataFreeq.Freeq)))
 
WorkingDataFreeq['FreeqIndex'] = c
 
WorkingDataFreeq.head()


# In[6]:


#Recency of Visit
WorkingDataRecency = cp.deepcopy(WorkingData)

WorkingDataRecency['JobRunDate'] = datetime.datetime.now()

# Function for Duration Calculator
def udf_duration(StartDate, EndDate):
    return ((StartDate - EndDate).days)

duration = pd.Series(list(map(lambda x,y : udf_duration(x,y),WorkingDataRecency.JobRunDate,WorkingDataRecency.End)))

WorkingDataRecency['Duration'] = duration

WorkingDataRecency = WorkingDataRecency.loc[:,['Name','Duration']].sort_values('Duration').groupby('Name').first()

WorkingDataRecency = WorkingDataRecency.reset_index()

WorkingDataRecency.loc[WorkingDataRecency.loc[:,'Duration']<0,:] = 1

dbins = np.histogram(WorkingDataRecency.Duration,bins=5)[1]

def udf_recencyIndex(duration):
    range_array = dbins
    recencyIndex = 1
    if duration < range_array[1]:
        recencyIndex = 1
    elif duration >= range_array[len(range_array)-2]:
        recencyIndex = len(range_array)-1
    else:
        for k in range(1,len(range_array)-1):
            if duration >= range_array[k] and duration < range_array[k+1]:
                recencyIndex = k+1
    return(recencyIndex)

dur = pd.Series(list(map(lambda x : udf_recencyIndex(x),WorkingDataRecency.Duration)))

WorkingDataRecency['RecencyIndex'] = dur
 
WorkingDataRecency.head()


# In[7]:


## Monitory Score

WorkingDataMonitory = cp.deepcopy(WorkingData)

WorkingDataMonitory  =(WorkingDataMonitory.groupby('Name').sum()/sum(WorkingDataMonitory.Rental))*100

WorkingDataMonitory = WorkingDataMonitory.reset_index()

 
def udf_monitoryIndex(price):
    pIndex = 1
    if price > .75:
        pIndex = 5
    elif price <=.75 and price >.1:
        pIndex = 4
    elif price <=.1 and price >.01:
        pIndex = 3
    elif price <=.01 and price >.001:
        pIndex = 2
    else:
        pIndex = 1
    return(pIndex)

Mbins = np.histogram(WorkingDataMonitory.Rental, 5)[1]

mon = pd.Series(list(map(lambda x : udf_monitoryIndex(x),WorkingDataMonitory.Rental)))
 
WorkingDataMonitory['monitoryIndx'] = mon
 
WorkingDataMonitory.tail()


# In[8]:


#Putting it all together
RM = pd.merge(WorkingDataRecency,WorkingDataMonitory, on='Name')

RFM_c = pd.merge(RM, WorkingDataFreeq, on='Name')

RFM = RFM_c.loc[:,['Name','RecencyIndex','FreeqIndex','monitoryIndx']]

RFM['RFM'] = RFM.RecencyIndex.astype('str').str.cat(RFM.FreeqIndex.astype('str').str.cat(RFM.monitoryIndx.astype('str')))


# In[18]:


RFM


# In[13]:


RFM['Segments'] = 'OV'


# In[35]:


for idx in RFM.index:
    RFM.iloc[idx,5] = 'VLHV' if RFM.iloc[idx,4] in ('444','445','455','555','554','544') else 'VLLV' if RFM.iloc[idx,4] in ('441','442','443','451','452','453','551','552','553','541','542','543') else 'NCHV' if (RFM.iloc[idx,4][0] in ('4','5') and RFM.iloc[idx,4][2] in ('4', '5')) else 'VFHV' if (RFM.iloc[idx,4][1] in ('4','5') and RFM.iloc[idx,4][2] in ('4', '5')) else 'OV'


# In[36]:


np.unique(RFM.Segments)


# In[37]:


RFM.to_csv("RFM_Score_.csv", index=False,header=1, encoding='utf-8')

