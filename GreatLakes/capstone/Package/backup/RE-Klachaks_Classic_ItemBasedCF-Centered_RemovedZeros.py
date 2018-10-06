
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import datetime
import random
from scipy import spatial
import math
import json
import copy as cp
from matplotlib import pyplot
import re

get_ipython().magic('matplotlib inline')

from pymongo import MongoClient
mdbClient = MongoClient('localhost',27017)


# In[2]:


t0 = datetime.datetime.now()


# In[3]:


# Getting Data from Klachaks
srcData = pd.read_csv("C:\Home\Work\GreatLakes\capstone\withOldData\LatestDataFromKlachaks.csv")


# In[4]:


#formatting it for CF, what we need is only Customer Name and Product
srcData.head()

WorkingData = cp.deepcopy(srcData)

WorkingData = WorkingData.loc[:,['Customer_Name','ProductName']]

WorkingData['count'] = 1

WorkingData.columns = ['customer','product','count']

t1 = datetime.datetime.now()
for prd in range(len(WorkingData)):
    WorkingData.iloc[prd,1] = re.sub('[^A-Za-z0-9]+',' ',WorkingData.iloc[prd,1])
print(datetime.datetime.now() - t1)

item_user_df = cp.deepcopy(WorkingData)


# In[252]:


#RFM Score
RFM_Score = pd.read_csv("RFM_Score_.csv")


# In[253]:


RFM_Score.head()


# In[5]:




t1 = datetime.datetime.now()
# Assign the Rating Based on    of time user has purchased that product
item_user_count = item_user_df.groupby(['customer','product']).aggregate(sum)
item_user_count = item_user_count.reset_index()
item_user_ratings = item_user_count
item_user_ratings['Ratings']=np.nan


#Randomly Assign the ratings
ratingScale = np.array([1,2,3,4,5])
for idx in item_user_ratings.index:
    item_user_ratings.iloc[idx,3] = np.random.choice(ratingScale,1, p=[0.4,0.45,0.1,0.05,0.0])[0] if item_user_ratings.iloc[idx,2] ==1 else (np.random.choice(ratingScale,1, p=[0.05,0.2,0.4,0.3,0.05])[0] if item_user_ratings.iloc[0,2] == 2 else np.random.choice(ratingScale,1, p=[0.0,0.15,0.35,0.35,0.15])[0])

print(datetime.datetime.now() - t1)



# In[7]:


item_user_ratings.tail()


# In[8]:



# Centering the Rating
t1 = datetime.datetime.now()
AverageRating = item_user_ratings.loc[:,['product','Ratings']].groupby('product').mean()
AverageRating = AverageRating.reset_index()
item_user_ratings = pd.merge(item_user_ratings,AverageRating, on='product', how='left')
item_user_ratings.columns = ['customer','product','count','OriginalRatings','AverageRatings']
item_user_ratings['CenteredRatings'] = item_user_ratings.loc[:,'OriginalRatings'] - item_user_ratings.loc[:,'AverageRatings']
item_user_ratings['OriginalRatings'] = item_user_ratings['OriginalRatings'].astype('Int64')
print(datetime.datetime.now() - t1)



# In[9]:


# TO AVOID DIVISION BY ZERO ERROR, WE WILL REPLACE CenteredRatings 0 WITH 0.0001
item_user_ratings.loc[item_user_ratings.loc[:,'CenteredRatings']==0,'CenteredRatings'] = 0.0001


# In[10]:



# Create Item User Matrix with original rating and to be predircted ratings [this is used for measuring similarity]
t1 = datetime.datetime.now()
unique_products = np.unique(item_user_ratings['product'])
unique_customers = np.unique(item_user_ratings['customer'])
s_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)
o_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)
p_user_item_matrix = pd.DataFrame(0, columns=unique_customers, index=unique_products)

for c in unique_customers:
    s_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'OriginalRatings'])
    o_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'OriginalRatings'])
    p_user_item_matrix.loc[item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'product'],c] = np.array(item_user_ratings.loc[item_user_ratings.loc[:,'customer'] == c,'CenteredRatings'])
print(datetime.datetime.now() - t1) 


# In[18]:


sparcePercentage


# In[14]:


# Sparcity Level
#Overall Rating Sparicity
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



# In[23]:


pyplot.plot(range(len(sparcityDF)),sparcityDF.Sparcity)


# In[197]:


pyplot.boxplot(sparcityDF.Sparcity, vert=False)


# In[24]:


#Calculating Cosine Similarity Index using p_user_itemMatrix
t1 = datetime.datetime.now()
item_item_matrix = pd.DataFrame(np.nan, columns=unique_products, index=unique_products)

for prd1 in unique_products:
    vec1 = p_user_item_matrix.loc[prd1,:]
    for prd2 in unique_products:
        vec2 = p_user_item_matrix.loc[prd2,:]
        item_item_matrix.loc[prd1,prd2] = 1-spatial.distance.cosine(vec1,vec2)
print(datetime.datetime.now() - t1)        



# In[28]:


item_item_matrix.head()


# In[25]:


#Rating Prediction
def FindNeighborAndRelativeWeight(user,item,items,n):
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
    OriginalRating = np.array(o_user_item_matrix.loc[SimilarProducts,user])
    
    #Predict the Score using Weighted Average
    SumSimilarities = sum(SimilarityScore) if sum(SimilarityScore) != 0 else .0001
    p_rating = np.dot(SimilarityScore,OriginalRating)/SumSimilarities
    
    #o_user_item_matrix.loc[:,113]
    return(math.ceil(p_rating))
    #return(np.sort(item_item_matrix.loc[items,item],axis=-1,kind='quicksort')[::-1][:k])
    


# In[29]:


t1 = datetime.datetime.now()
# k - will hol dthe index keys for nonzero rating of the iser 'user'
# O_U_I_U_R_I = Original User Item User Rated Item - Will hold the User rated Item' Ids
for item in o_user_item_matrix.index:
    for user in o_user_item_matrix.columns:
        if o_user_item_matrix.loc[item,user] == 0:
            k = np.nonzero(o_user_item_matrix.loc[:,user])
            O_U_I_U_R_I = o_user_item_matrix.index[k]
            o_user_item_matrix.loc[item,user] = FindNeighborAndRelativeWeight(user,item,O_U_I_U_R_I,4)
            

print(datetime.datetime.now() - t1)


# In[30]:


item_user_rating_complete = pd.DataFrame()


# In[31]:


item_user_rating_temp = pd.DataFrame()


# In[32]:


custName = unique_customers


# In[33]:


t1 = datetime.datetime.now()
for prod in unique_products:
    predRating = list(o_user_item_matrix.loc[prod,:])
    srcRating = list(s_user_item_matrix.loc[prod,:])
    temp_df = pd.DataFrame({'customer': custName,'product':prod,'PrediectedRating': predRating,'SourceRating':srcRating} )
    item_user_rating_temp = item_user_rating_temp.append(temp_df)
print(datetime.datetime.now() - t1)


# In[35]:


item_user_rating_temp.head(10)


# In[36]:


item_user_rating_complete = item_user_rating_temp


# In[37]:


item_user_rating_complete['predicted'] = 'Y' 


# In[38]:


item_user_rating_complete.loc[item_user_rating_complete.loc[:,'SourceRating']!=0,'predicted'] = 'N'


# In[39]:


item_user_rating_complete = item_user_rating_complete.loc[:,['customer','product','PrediectedRating','predicted']]


# In[40]:


item_user_rating_complete.columns = ['customer', 'product', 'rating', 'predicted']


# t1 = datetime.datetime.now()
# k = 1
# for u in unique_customers[1:10]:
#     for p in unique_products[1:10]:
#         customer = u
#         product = p
#         predicted = 'N' if s_user_item_matrix.loc[product,customer] != 0 else 'Y'
#         item_user_rating_complete = item_user_rating_complete.append({'customer':customer,'product':product, 'rating' :o_user_item_matrix.loc[product,customer],'predicted':predicted }, ignore_index=True)       
#         print("item : %s | user : %s | loop  number : %d" % (p,u,k))
#         k = k + 1
#     
# print(datetime.datetime.now() - t1)

# In[41]:


len(item_user_rating_complete)


# In[42]:


t1 = datetime.datetime.now()
u_ = range(len(o_user_item_matrix.columns))
p_ = range(len(o_user_item_matrix.index))
## Insert Data to Mongo DB
db = mdbClient.RE
for c in db.collection_names():
    db[c].drop()


# In[43]:


col_item_user_rating = db.create_collection('item_user_rating')
col_item_item_matrix = db.create_collection('item_item_matrix')


# In[45]:


sample = item_user_rating_complete


# In[46]:


sample = sample.reset_index()


# In[47]:


dict_item_user_rating_complete = sample.T.to_dict().values()
item_item_matrix_ = item_item_matrix.reset_index()
item_item_matrix_.columns = item_item_matrix_.columns.astype('str')
dict_item_item_matrix_complete = item_item_matrix_.T.to_dict().values()


# In[48]:


col_item_user_rating.insert_many(dict_item_user_rating_complete)
col_item_item_matrix.insert_many(dict_item_item_matrix_complete)


# In[218]:


print(datetime.datetime.now()-t0)


# In[1]:


# Model Evaluaiton
item_user_rating_complete.head()

