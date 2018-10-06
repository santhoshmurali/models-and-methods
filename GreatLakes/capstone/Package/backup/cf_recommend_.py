
# coding: utf-8

# In[1]:

import sys
import pymongo
from pymongo import MongoClient
import pprint
import numpy as np
import pandas as pd
import copy as cp

# In[2]:
client = MongoClient('localhost',27017)
database = client.RE

# In[5]:
col_item_user_rating = database.item_user_rating
col_item_item_matrix = database.item_item_matrix

argument = sys.argv
# In[43]:
def recommend(n_customer,n_product,n_similarities):
    n_customer = 3 #int(argument[1])
    n_product = 11 #int(argument[2])
    n_similarities = 5 #int(argument[3])
    temp_similarity_df = pd.DataFrame()
    l1 = list(database['item_item_matrix'].find({},{"_id":0,str(n_product):1,"index":1}).sort(str(n_product),pymongo.DESCENDING).limit(n_similarities).skip(1))
    temp_similarity_df= pd.DataFrame(l1)
    similarity_Array = (temp_similarity_df['index'])
    similarity_Array = list(similarity_Array.values)
    recomended_Products =  list(database['item_user_rating'].find({"$and":[{"customer":n_customer},{"product":{"$in":similarity_Array}},{"predicted":"Y"}]},{"_id":0,"product":1,"rating":1}).sort('rating',pymongo.DESCENDING))
    recomended_Products_df = pd.DataFrame(recomended_Products)
    df_similarity_score = cp.deepcopy(temp_similarity_df)
    df_similarity_score.columns = ['Similarity', 'product']
    df_similarity_score['product'] = df_similarity_score['product'].astype('Int64')
    recomendation_df = pd.merge(recomended_Products_df, df_similarity_score, on='product', how='left')
    recomendation_df['UserId'] = n_customer
    recomendation_df = recomendation_df.loc[:,['UserId','product','Similarity','rating']]
    recomendation_df.columns = ['UserID','ProductID','Similarity','Ratings']
    return(recomendation_df.T.to_dict())

print(recommend(argument[1],argument[2],argument[3]))
