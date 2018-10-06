
# coding: utf-8

# # Assignment Topic: Making Election Predictions  
# 
# ##### If during the early part of 2014 you had to predict the upcoming Lok Sabha Elections, what would be your predictive model?  (Say you wanted to predict the election outcomes six months before the elections were held)
# 
# To solve this problem…here are some possible stuff you can think of….
# 
# * Identify data sources. This will depend upon what factors you think can determine winning elections
# * Identifying the right technique/techniques to model that data
# * Make predictions, validate and update your model.
# 
# ## Submission details:
# 
# ### Part 1: 
# #### Identify the data sources, the variables of interest. Process the data and create a ‘clean’ data file(s).
# 
# ##### Marks distribution: 20.
# 
# #### What needs to be submitted:
# 
# * Text file explaining what variables you are choosing and why, the source codes, explanation etc
# * The cleaned data file (s)
# * Relevant r codes if any
# 
# Marks will be awarded based on what factors you think are important in model building, what data sources you identify and finally how the clean data looks. The clean data has to be submitted as CSV (files).
# 
# ### Part 2: 
# #### Develop the model,present a predictive model.
# 
# ##### Marks distribution: 20
# 
# #### What needs to be submitted:
# 
# * Text file explaining the model (s), why did you do what you did, model performance et
# * Relevant R codes if any
#  
# 

# In[1]:


import numpy as np
import pandas as pd


# ## Part 1
# * Identify the data sources
# * Variables of interest. 
# * Process the data and create a ‘clean’ data file(s).
# 

# ### Data Sources
# 
# #### use the data from the Website http://eci.nic.in/eci_main1/ElectionStatistics.aspx
# We are using the data from 2009 election results and predecting 2014 Election results, assuming the Prediciton is done 6 months before 2014 election
# 
# ##### The Data we took it is 
# * GENERAL ELECTION TO LOKSABHA, 2009, http://eci.nic.in/eci_main/StatisticalReports/candidatewise/GE2009.xls
#  * Electors
#  * Candidate Wise results.

# In[2]:


# Importing Data Directly from the website
SrcDataElectors = pd.read_excel("http://eci.nic.in/eci_main/StatisticalReports/candidatewise/GE2009.xls","electors", 
                                skiprows=3)
SrcDataCandidates = pd.read_excel("http://eci.nic.in/eci_main/StatisticalReports/candidatewise/GE2009.xls","Cand_Wise", 
                                skiprows=5)


# ##### The data is from all States. Need to filterout only Uttar Pradesh data for Analysis.

# In[3]:


SrcDataElectors.loc[:,"STATE"].value_counts()


# In[16]:


SrcDataCandidates.loc[:,"State name"].value_counts()


# In[4]:


#UPCandidates

UPElectors = SrcDataElectors.loc[SrcDataElectors["STATE"] =='Uttar Pradesh',:]
UPCandidates = SrcDataCandidates.loc[SrcDataCandidates["State name"]=='Uttar Pradesh',:]


# In[5]:


UPElectors["STATE"].value_counts()


# In[25]:


UPCandidates["State name"].value_counts()


# In[6]:


UPElectors.columns


# In[28]:


UPCandidates.columns


# #### Joining the Candidates and Electors data to get one holistic Data Frame

# In[7]:


temp = pd.merge(UPCandidates, UPElectors, how='outer', left_on='PC Number', right_on='PC NO')


# In[8]:


temp.columns


# In[9]:


UP2009BASEDATA = temp.loc[:,[ 'STATE CODE', 'State name', 'PC Number', 'PC name',
       'PC Type', 'Candidate Name', 'Candidate Sex', 'Candidate Category',
       'Candidate Age', 'Party Abbreviation', 'Total Votes Polled', 'Position',
       'Total voters', 'Total_Electors', 'TOT_CONTESTANT', 'POLL PERCENTAGE']]


# In[10]:


UP2009BASEDATA


# In[39]:


UP2009BASEDATAWINNERS = UP2009BASEDATA.loc[UP2009BASEDATA.Position == 1,:]


# In[40]:


UP2009BASEDATAWINNERS = UP2009BASEDATAWINNERS.loc[:,['STATE CODE','PC Number','Candidate Name','Party Abbreviation','Total_Electors','POLL PERCENTAGE']]


# In[42]:


UP2009BASEDATAWINNERS.columns = ['STATE_CODE 09','PC Number 09','Winning Candidate 09','Winning Party 09','Total_Electors 09','Winning Poll Percentage 09']
UP2009BASEDATAWINNERS 


# ### Import the data for 2014.

# In[17]:


SrcDataElectors2014 = pd.read_excel("http://eci.nic.in/eci_main/StatisticalReports/candidatewise/LS-2014_ElectionResult.xls","electors", 
                                skiprows=3)
SrcDataCandidates2014 = pd.read_excel("http://eci.nic.in/eci_main/StatisticalReports/candidatewise/LS-2014_ElectionResult.xls","Cand_Wise", 
                                skiprows=3)


# In[23]:


UPElectors2014 = SrcDataElectors2014.loc[SrcDataElectors2014.STATE == 'Uttar Pradesh',:]
UPCandidates2014 = SrcDataCandidates2014.loc[SrcDataCandidates2014["State name"]=='Uttar Pradesh',:]


# In[28]:


temp = pd.merge(UPCandidates2014, UPElectors2014, how='outer', left_on='PC Number', right_on='PC NO')
UP2014BASEDATA = temp.loc[:,[ 'STATE CODE', 'State name', 'PC Number', 'PC name',
       'PC Type', 'Candidate Name', 'Candidate Sex', 'Candidate Category',
       'Candidate Age', 'Party Abbreviation', 'Total Votes Polled', 'Position',
       'Total voters', 'Total_Electors', 'TOT_CONTESTANT', 'POLL PERCENTAGE']]


# In[48]:


UP2014TRAIN = pd.merge(UP2014BASEDATA,UP2009BASEDATAWINNERS, how='outer', left_on='PC Number', right_on='PC Number 09')


# In[50]:


UP2014TEST = UP2014TRAIN
UP2014TRAIN = UP2014TRAIN.loc[:,['STATE CODE', 'State name', 'PC Number', 'PC name', 'PC Type',
       'Candidate Name', 'Candidate Sex', 'Candidate Category',
       'Candidate Age', 'Party Abbreviation','Total_Electors', 'TOT_CONTESTANT', 'STATE_CODE 09', 'PC Number 09', 'Winning Candidate 09',
       'Winning Party 09','Total_Electors 09', 'Winning Poll Percentage 09']]


# In[51]:


UP2014TRAIN.head()


# In[ ]:




