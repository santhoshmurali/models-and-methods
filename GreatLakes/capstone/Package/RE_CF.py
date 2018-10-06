# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 23:35:56 2017

This wil not have Model Test and Eval using RMSE, but integrated with RFM.
# python RE_CF.py <xlsx file path>
@author: Santhosh
"""

# In[1]
# Required Libraries

import pandas as pd
import numpy as np
import datetime
import copy as cp
from scipy import spatial
import math
import random

from sys import argv
#for regression

#from sklearn.metrics import mean_squared_error, r2_score
#from sklearn.linear_model import LinearRegression

#get_ipython().magic('matplotlib inline')

from pymongo import MongoClient
mdbClient = MongoClient('localhost',27017)



#######  RFM Analysis
# In[2]: Source Data

random.seed(9001)

#path_to_data_excel = argv[1] 
salesData = pd.read_excel("C:\Home\Work\GreatLakes\capstone\Package\latestproducts_with_id.xlsx")
#salesData = pd.read_csv("C:\Home\Work\GreatLakes\capstone\Package\KlachacksTransData.csv")


# In[3]: Data Cleaning for RFM

RawData = pd.DataFrame()
RawData = salesData.loc[:,['UserID','EndDate','Rent2']]
WorkingData = cp.deepcopy(RawData)
WorkingData.Rental = WorkingData.Rent2.astype('int32')
WorkingData.EndDate = pd.to_datetime(WorkingData.EndDate, format='%m/%d/%Y %H:%M')
WorkingData.columns = ['UserID','EndDate','Rental']

# In[4]: To Find Freequency Index

## FreeQuency
WorkingDataFreeq = cp.deepcopy(WorkingData)
#Freequenct of visit
WorkingDataFreeq = WorkingDataFreeq.loc[:,['UserID','EndDate']].groupby('UserID').count()
WorkingDataFreeq = WorkingDataFreeq.reset_index()
WorkingDataFreeq.columns = ['UserID','Freeq']

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


# In[6]: To Find Recency Index

#Recency of Visit
WorkingDataRecency = cp.deepcopy(WorkingData)
WorkingDataRecency['JobRunDate'] = datetime.datetime.now()

# Function for Duration Calculator
def udf_duration(StartDate, EndDate):
    return ((StartDate - EndDate).days)

duration = pd.Series(list(map(lambda x,y : udf_duration(x,y),WorkingDataRecency.JobRunDate,WorkingDataRecency.EndDate)))

WorkingDataRecency['Duration'] = duration
WorkingDataRecency = WorkingDataRecency.loc[:,['UserID','Duration']].sort_values('Duration').groupby('UserID').first()
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


# In[7]: TO Find Monitory Index

## Monitory Score
WorkingDataMonitory = cp.deepcopy(WorkingData)
WorkingDataMonitory  =(WorkingDataMonitory.groupby('UserID').sum()/sum(WorkingDataMonitory.Rental))*100
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


# In[8]: Putting it all together

#Putting it all together
RM = pd.merge(WorkingDataRecency,WorkingDataMonitory, on='UserID')
RFM_c = pd.merge(RM, WorkingDataFreeq, on='UserID')
RFM = RFM_c.loc[:,['UserID','RecencyIndex','FreeqIndex','monitoryIndx']]
RFM['RFM'] = RFM.RecencyIndex.astype('str').str.cat(RFM.FreeqIndex.astype('str').str.cat(RFM.monitoryIndx.astype('str')))



# In[9]: Segmentation

RFM['Segments'] = 'OV'

# Categorizing
# 444,445,455,555,554,544 : Very Loyal and High Value Customers (VLHV)
# 4*5,5*4 : New Customers and High Value (NCHV)
# *45,*54 : Very Frequent and High Value Customers (VFHV) 
# 441,442,443,451,452,453,551,552,553,541,542,543 : Very Loyal and Low Value Customers (VLLV)
# Rest : Onetime Visitors.
for idx in RFM.index:
    RFM.iloc[idx,5] = 'VLHV' if RFM.iloc[idx,4] in ('444','445','455','555','554','544') else 'VLLV' if RFM.iloc[idx,4] in ('441','442','443','451','452','453','551','552','553','541','542','543') else 'NCHV' if (RFM.iloc[idx,4][0] in ('4','5') and RFM.iloc[idx,4][2] in ('4', '5')) else 'VFHV' if (RFM.iloc[idx,4][1] in ('4','5') and RFM.iloc[idx,4][2] in ('4', '5')) else 'OV'

np.unique(RFM.Segments)
RFM.to_csv("c:\\temp\\RFM_Score_.csv", index=False,header=1, encoding='utf-8')


# In[36]
# Building Similarity Matrix, Rating Predicitons

# Getting Data from Klachaks
srcData = cp.deepcopy(salesData)


# In[37]
srcData.head()
#formatting it for CF, what we need is only Customer Name and Product
WorkingData = cp.deepcopy(srcData)
WorkingData = WorkingData.loc[:,['UserID','ProductID']]
WorkingData['count'] = 1
WorkingData.columns = ['UserID','ProductID','count']
t1 = datetime.datetime.now()
#for prd in range(len(WorkingData)):
#    WorkingData.iloc[prd,1] = re.sub('[^A-Za-z0-9]+',' ',WorkingData.iloc[prd,1])
print(datetime.datetime.now() - t1)
item_user_df = cp.deepcopy(WorkingData)

# In[38]
RFM_Score = cp.deepcopy(RFM)
RFM_Score.head()

# In[39]
#### Assigning Rating - Derived from Sales and RFM
# Assign the Rating Based on    of time user has purchased that product
#  Item User Count
item_user_count = item_user_df.groupby(['UserID','ProductID']).aggregate(sum)
item_user_count = item_user_count.reset_index()
item_user_count_rating = cp.deepcopy(item_user_count)
item_user_count_rating['cnt_Rating'] = np.nan

ratingScale = np.array([1,2,3,4,5])
for idx in item_user_count_rating.index:
    item_user_count_rating.iloc[idx,3] = np.random.choice(ratingScale,1, p=[0.4,0.45,0.1,0.05,0.0])[0] if item_user_count_rating.iloc[idx,2] ==1 else (np.random.choice(ratingScale,1, p=[0.05,0.2,0.4,0.3,0.05])[0] if item_user_count_rating.iloc[0,2] == 2 else np.random.choice(ratingScale,1, p=[0.0,0.2,0.4,0.25,0.15])[0])
    print("Count Random Value %d" % item_user_count_rating.iloc[idx,3])
# User RFM
item_with_rfm = RFM_Score.loc[:,['UserID','Segments']]
item_with_rfm['rfm_Rating'] = np.nan
for idx in item_with_rfm.index:
    item_with_rfm.iloc[idx,2] = np.random.choice(ratingScale,1, p=[0.1,0.2,0.3,0.3,0.1])[0] if item_with_rfm.iloc[idx,1] == 'VLHV' else ( np.random.choice(ratingScale,1, p=[0.2,0.1,0.3,0.25,0.15])[0] if item_with_rfm.iloc[idx,1] == 'VLHV' else ( np.random.choice(ratingScale,1, p=[0.25,0.2,0.2,0.2,0.15])[0] if item_with_rfm.iloc[idx,1] == 'VFHV' else ( np.random.choice(ratingScale,1, p=[0.1,0.2,0.35,0.15,0.2])[0] if item_with_rfm.iloc[idx,1] == 'VLLV'	else np.random.choice(ratingScale,1, p=[0.35,0.35,0.15,0.1,0.05])[0])))
    print("RFM Random Value %d" % item_with_rfm.iloc[idx,2])
 
    
item_user_ratings_temp = pd.merge(item_user_count_rating, item_with_rfm, on='UserID',how='left')
item_user_ratings_temp['Ratings'] = (item_user_ratings_temp.cnt_Rating+item_user_ratings_temp.rfm_Rating)/2
item_user_ratings_temp['Ratings'] = np.round(item_user_ratings_temp['Ratings'])

item_user_ratings = cp.deepcopy(item_user_ratings_temp)
item_user_ratings = item_user_ratings.loc[:,['UserID','ProductID','Ratings']]


# In[40]
# Centering the Rating
AverageRating = item_user_ratings.loc[:,['ProductID','Ratings']].groupby('ProductID').mean()
AverageRating = AverageRating.reset_index()
item_user_ratings = pd.merge(item_user_ratings,AverageRating, on='ProductID', how='left')
item_user_ratings.columns = ['customer','product','OriginalRatings','AverageRatings']
item_user_ratings['CenteredRatings'] = item_user_ratings.loc[:,'OriginalRatings'] - item_user_ratings.loc[:,'AverageRatings']
item_user_ratings['OriginalRatings'] = np.nan_to_num(item_user_ratings['OriginalRatings'])
item_user_ratings['OriginalRatings'] = item_user_ratings['OriginalRatings'].astype('Int64')

# In[41]:
# For Building Test Matrix
#len(item_user_ratings)
#TestArray = np.random.choice(range(round(len(item_user_ratings)*.6)),round(len(item_user_ratings)*.6),replace=False)
#item_user_ratings_t = cp.deepcopy(item_user_ratings)
#item_user_ratings_t = item_user_ratings_t.iloc[TestArray,:]
#item_user_ratings_t.shape

# In[42]:
# TO AVOID DIVISION BY ZERO ERROR, WE WILL REPLACE CenteredRatings 0 WITH 0.0001
item_user_ratings.loc[item_user_ratings.loc[:,'CenteredRatings']==0,'CenteredRatings'] = 0.0001

# In[42]:
# Create Item User Matrix with original rating and to be predircted ratings [this is used for measuring similarity]
unique_products = np.unique(item_user_ratings['product'])
unique_customers = np.unique(item_user_ratings['customer'])
#Test Data Prepration
training_data_len = int(np.round(len(unique_customers)*.7,0))
test_data_len = int(round(len(unique_customers)) - training_data_len)

training_users = random.sample(list(unique_customers),training_data_len)
test_users = random.sample(list(unique_customers),test_data_len)


s_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)
o_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)

train_user_item_matrix = pd.DataFrame(0, columns=training_users, index=unique_products)
test_user_item_matrix = pd.DataFrame(0, columns=test_users, index=unique_products)

p_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)

#Test Set
#t_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)

for c in unique_customers:
    s_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'OriginalRatings'])
    o_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'OriginalRatings'])
    p_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'CenteredRatings'])
    
    train_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'OriginalRatings'])
    test_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'OriginalRatings'])
    
    #Test Set
    #t_user_item_matrix.loc[item_user_ratings_t.loc[item_user_ratings_t.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings_t.loc[item_user_ratings_t.loc[:,'customer'] == c,'OriginalRatings'])

# In[43]:
# Sparcity Level
#Overall Rating Sparicity : Original Data
ratedCount = np.count_nonzero(s_user_item_matrix)
matrixSize = s_user_item_matrix.shape[0]*s_user_item_matrix.shape[1]
sparcePercentage = (ratedCount/matrixSize)*100

sparcityDF = pd.DataFrame()

#Item wise rating Sparcity and Variance from Overall Sparcity.
TotalUsers = len(unique_customers)
for prd in unique_products:
    sparcityDF = sparcityDF.append({'product':prd, 'Sparcity':(np.count_nonzero(s_user_item_matrix.loc[prd,:])/TotalUsers)*100}, ignore_index=True)

# % of products that are lesser than Overall Sparcity
(len(sparcityDF.loc[sparcityDF.loc[:,'Sparcity']<sparcePercentage,:])/len(unique_products))*100

# In[44]:
# Sparcity Level
#Overall Rating Sparicity : Test data
#ratedCount = np.count_nonzero(t_user_item_matrix)
#matrixSize = t_user_item_matrix.shape[0]*t_user_item_matrix.shape[1]
#sparcePercentage = (ratedCount/matrixSize)*100

#sparcityDF = pd.DataFrame()

#Item wise rating Sparcity and Variance from Overall Sparcity.
#TotalUsers = len(unique_customers)
#for prd in unique_products:
#    sparcityDF = sparcityDF.append({'product':prd, 'Sparcity':(np.count_nonzero(s_user_item_matrix.loc[prd,:])/TotalUsers)*100}, ignore_index=True)

# % of products that are lesser than Overall Sparcity
#(len(sparcityDF.loc[sparcityDF.loc[:,'Sparcity']<sparcePercentage,:])/len(unique_products))*100

# In[45]:
#Calculating Cosine Similarity Index using p_user_itemMatrix
with open("c:\\temp\\item_item_log.txt","a") as itemitemLogFile:
    t1 = datetime.datetime.now()
    item_item_matrix = pd.DataFrame(np.nan, columns=unique_products, index=unique_products)
    #p_user_item_matrix_bkUp = cp.deepcopy(p_user_item_matrix)
    #p_user_item_matrix = pd.DataFrame(np.nan_to_num(p_user_item_matrix))
    
    for prd1 in unique_products:
        vec1 = np.nan_to_num(p_user_item_matrix.loc[prd1,:])
        itemitemLogFile.writelines("-" * 10 + "\n")
        itemitemLogFile.writelines("Product 1 %s" % prd1 + "\n")
        itemitemLogFile.writelines(str(vec1) + "\n")
        itemitemLogFile.writelines(str(np.nan_to_num(p_user_item_matrix.loc[prd1,:])) + "\n")
        itemitemLogFile.writelines("-" * 5 + "\n")
        for prd2 in unique_products:
            vec2 = np.nan_to_num(p_user_item_matrix.loc[prd2,:] )
            itemitemLogFile.writelines("-" * 5 + "\n")
            itemitemLogFile.writelines("Product 2 %s" % prd2 + "\n")
            itemitemLogFile.writelines(str(vec2) + "\n")
            itemitemLogFile.writelines(str(np.nan_to_num(p_user_item_matrix.loc[prd2,:])) + "\n")
            itemitemLogFile.writelines("-" * 5 + "\n")
            item_item_matrix.loc[prd1,prd2] = 1-spatial.distance.cosine(vec1,vec2)
    print(datetime.datetime.now() - t1)   

# In[46]:
#Rating Prediction : Original Data
def FindNeighborAndRelativeWeight(user,item,items,n,ItemUserMatrix):
    # This will return all the Similar Items that are rated related to the Selected item, 'item'
    ItemSimilarities = pd.DataFrame(item_item_matrix.loc[items,item])
    ItemSimilarities = ItemSimilarities.reset_index()
    ItemSimilarities.columns = ['product_c','similarity']
    # We will sort the item decending to have the most similar item first
    ItemSimilarities = ItemSimilarities.sort_values('similarity',ascending=False)
    # Based on the Number of similar neigbors (n), we can select those.
    ItemSimilarities = ItemSimilarities.head(n)
                                  
    # We have totake the rating of Most similar product. To do that, we have to take the product id of most similar product and pass it back
    # pass it basck to user item matrix to get the rating. Mind that the order has to be same.
    # Otherwise sum product will go wrong
    SimilarProducts = np.array(ItemSimilarities.loc[:,'product_c'])
    
    #Identify Similarity Score and Original Rating
    SimilarityScore = np.array(ItemSimilarities.loc[:,'similarity'])
    OriginalRating = np.array(ItemUserMatrix.loc[SimilarProducts,user])
    
    #Predict the Score using Weighted Average
    SumSimilarities = sum(SimilarityScore) if sum(SimilarityScore) != 0 else .0001
    p_rating = np.nan_to_num(np.dot(SimilarityScore,OriginalRating)/SumSimilarities)
    
    #o_user_item_matrix.loc[:,113]
    return(math.ceil(p_rating))
    #return(np.sort(item_item_matrix.loc[items,item],axis=-1,kind='quicksort')[::-1][:k])

# In[49]: 
# Predicting the Rating from Rest if the items for which user has not rated
# k - will hold the index keys for nonzero rating of the iser 'user'
# O_U_I_U_R_I = Original User Item User Rated Item - Will hold the User rated Item' Ids
for item in o_user_item_matrix.index:
    for user in o_user_item_matrix.columns:
        if o_user_item_matrix.loc[item,user] == 0:
            k = np.nonzero(o_user_item_matrix.loc[:,user])
            O_U_I_U_R_I = o_user_item_matrix.index[k]
            o_user_item_matrix.loc[item,user] = FindNeighborAndRelativeWeight(user,item,O_U_I_U_R_I,2)

for item in train_user_item_matrix.index:
    for user in train_user_item_matrix.columns:
        if train_user_item_matrix.loc[item,user] == 0:
            k = np.nonzero(train_user_item_matrix.loc[:,user])
            O_U_I_U_R_I = train_user_item_matrix.index[k]
            train_user_item_matrix.loc[item,user] = FindNeighborAndRelativeWeight(user,item,O_U_I_U_R_I,2,train_user_item_matrix)            

for item in test_user_item_matrix.index:
    for user in test_user_item_matrix.columns:
        if test_user_item_matrix.loc[item,user] == 0:
            k = np.nonzero(test_user_item_matrix.loc[:,user])
            O_U_I_U_R_I = test_user_item_matrix.index[k]
            test_user_item_matrix.loc[item,user] = FindNeighborAndRelativeWeight(user,item,O_U_I_U_R_I,2,test_user_item_matrix)            
# In[50]:
### Model Evaluation : Test Set
#t1 = datetime.datetime.now()
# k_t - will hol dthe index keys for nonzero rating of the iser 'user'
# O_U_I_U_R_I = Original User Item User Rated Item - Will hold the User rated Item' Ids
#for item_t in t_user_item_matrix.index:
#    for user_t in t_user_item_matrix.columns:
#        if t_user_item_matrix.loc[item_t,user_t] == 0:
#            k_t = np.nonzero(t_user_item_matrix.loc[:,user_t])
#            O_U_I_U_R_I_t = t_user_item_matrix.index[k_t]
#            t_user_item_matrix.loc[item_t,user_t] = FindNeighborAndRelativeWeight_t(user_t,item_t,O_U_I_U_R_I_t,2)

# In[51]
# In[Building Item_user_matrix for Train and Test]
def build_item_user_matrix(itemUserMatrix,prodList):
    item_user_rating_temp = pd.DataFrame()
    custName = itemUserMatrix.columns
    unique_products = prodList
    for prod in unique_products:
        predRating = list(itemUserMatrix.loc[prod,:])
        srcRating = list(s_user_item_matrix.loc[prod,:])
        #t_Rating = list(t_user_item_matrix.loc[prod,:])
        temp_df = pd.DataFrame({'customer': custName,'product':prod,'PrediectedRating': predRating,'SourceRating':srcRating} )
        #temp_df_t = pd.DataFrame({'customer': custName,'product':prod,'PrediectedRating': predRating,'SourceRating':srcRating, 'TestRating': t_Rating} )
        item_user_rating_temp = item_user_rating_temp.append(temp_df)
    item_user_rating_temp=item_user_rating_temp.fillna(0)
    item_user_rating_temp['predicted'] = 'Y'
    item_user_rating_temp.loc[item_user_rating_temp.loc[:,'SourceRating']!=0,'predicted'] = 'N'
    item_user_rating_temp = item_user_rating_temp.loc[:,['customer','product','PrediectedRating','predicted']]
    item_user_rating_temp.columns = ['customer', 'product', 'rating', 'predicted']
    item_user_rating_temp.customer = item_user_rating_temp['customer'].astype('Int64')
    return(item_user_rating_temp)

# In[51] : Transporsing the results to User Item Matrix format
# In[53]: Model Evaluation Section
#Evaluation
#item_user_rating_eval = cp.deepcopy(item_user_rating_temp_test)
#item_user_rating_eval['predicted'] = 'Y'
#item_user_rating_eval.loc[item_user_rating_eval.loc[:,'SourceRating']!=0,'predicted'] = 'N'
#item_user_rating_eval = item_user_rating_eval.loc[item_user_rating_eval.loc[:,'TestRating'] >= 0,:]
#item_user_rating_eval = item_user_rating_eval.loc[item_user_rating_eval.loc[:,'predicted'] == 'Y',:]
#item_user_rating_eval.loc[item_user_rating_eval.loc[:,'SourceRating'] > 0,:]
#np.sqrt(sum(((item_user_rating_eval.SourceRating - item_user_rating_eval.TestRating)**2)/len(item_user_rating_eval)))
#item_user_rating_eval = item_user_rating_eval.loc[item_user_rating_eval.loc[:,'TestRating'] >= 0,:]

# In[Write the data in Mongo]
def wrtie_to_Mongo(userItemMatrix,userItemRatingMatrix,types):
    u_ = range(len(userItemMatrix.columns))
    p_ = range(len(userItemMatrix.index))
    mdb_item_user_rating = types + '_item_user_rating'
    ## Insert Data to Mongo DB
    db = mdbClient.RE
    db[mdb_item_user_rating].drop()
    db['item_item_matrix'].drop()
    
    col_item_user_rating = db.create_collection(mdb_item_user_rating)
    col_item_item_matrix = db.create_collection('item_item_matrix')
    
    sample = userItemRatingMatrix
    sample = sample.reset_index()

    dict_userItemRatingMatrix = sample.T.to_dict().values()
    item_item_matrix_ = item_item_matrix.reset_index()
    item_item_matrix_.columns = item_item_matrix_.columns.astype('str')
    dict_item_item_matrix_complete = item_item_matrix_.T.to_dict().values()
    
    # Inserting the data
    col_item_user_rating.insert_many(dict_userItemRatingMatrix)
    col_item_item_matrix.insert_many(dict_item_item_matrix_complete)




