
# coding: utf-8

# In[1]:


import pymongo
from pymongo import MongoClient
import pprint
import numpy as np
import pandas as pd


# In[2]:


client = MongoClient('localhost',27017)


# In[3]:


database = client.RE


# In[5]:


col_item_user_rating = database.item_user_rating
col_item_item_matrix = database.item_item_matrix


# In[31]:


n_customer = 'vivek'
n_product = '5 in 1 Reflector Disc'
n_similarities = 5
temp_similarity_df = pd.DataFrame()
l1 = list(database['item_item_matrix'].find({},{"_id":0,str(n_product):1,"index":1}).sort(str(n_product),pymongo.DESCENDING).limit(n_similarities).skip(1))
temp_similarity_df= pd.DataFrame(l1)


# In[33]:


#temp_similarity_df['index'] = temp_similarity_df['index'].astype('int')

similarity_Array = (temp_similarity_df['index'])

similarity_Array = list(similarity_Array.values)


# In[36]:


recomended_Products =  list(database['item_user_rating'].find({"$and":[{"customer":n_customer},{"product":{"$in":similarity_Array}},{"predicted":"Y"}]},{"_id":0,"product":1,"rating":1}).sort('rating',pymongo.DESCENDING))

pd.DataFrame(recomended_Products)


