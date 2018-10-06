# -*- coding: utf-8 -*-
"""
Created on Wed Dec  6 09:45:06 2017

@author: Santhosh
"""

import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel
from sys import argv
import numpy as np
from collections import Counter
import pymongo
from pymongo import MongoClient
import copy as cp
from matplotlib import pyplot as pyplt


#Preprocessing
# Loading transactions data
#path_to_data_excel = argv[1] 
raw_data = pd.read_excel("C:\Home\Work\GreatLakes\capstone\Package\latestproducts_with_id.xlsx")
#raw_data = pd.read_excel(path_to_data_excel)
products = raw_data.groupby(['ProductID','ProductName']).size().to_frame().reset_index().drop(0,axis=1)
products.head()

#PreProcessing for Scoring and Metrics
client = MongoClient('localhost',27017)
database = client.RE
col_item_user_rating = database.item_user_rating
col_item_item_matrix = database.item_item_matrix
User_Id = int(argv[2])
Prod_id = int(argv[3])
Knear = int(argv[4])




#Recommendations
# In[1]
# ITEM BASED RECOMMENDATION
# -----------------------------
def item_recommend(n_customer,n_product,n_similarities):
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


def item_recommend_eval(n_customer,n_product,n_similarities):
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
    return(recomendation_df.T.to_dict(),)    

# In[4]
# Returning Good Product
def goodProd():
    all_prd = pd.DataFrame(list(col_item_user_rating.find()))
    good_prd = all_prd.loc[all_prd.loc[:,'predicted']=='N',:]
    unique_good_product = np.unique(good_prd['product'])
   # to_recomend = all_prd.loc[all_prd.loc[:,'predicted']=='Y',:]
    good_prod = set(good_prd.loc[good_prd.loc[:,'rating']>3,'product'])
    return(good_prod,unique_good_product)

# Number of Products
def countProductIDs(product_scores):
    count = Counter(i[0] for i in product_scores)
    return count

# Hits - Product that are good and recomended
def calculate_hits(predictions):
    goodProdS = goodProd()
    good_ids = goodProdS[0]
    
    hits = 0

    for id in predictions:
        for gid in good_ids:
            if(int(id) == gid):
                hits += 1

    return hits,len(good_ids)

# Calucalte Precision and Recall
def calculate_metrics(pred_to_recommend):
    hits,goodCount = calculate_hits(pred_to_recommend)

    precision = hits/len(pred_to_recommend)
    recall = hits/goodCount

    return precision,recall

# Calculate Precision and Recall for Hybrid
def calculate_hybrid_metrics(content_to_recommend,item_to_recommend):
    hybrid_to_recommend = [id for id in content_to_recommend if id in item_to_recommend]

    hits,good_count = calculate_hits(hybrid_to_recommend)
    
    precision = hits/len(hybrid_to_recommend)
    recall = hits/good_count
    
    return precision,recall

# Item Based Evaluation
def jp_eval_all():
    good_prod,unique_good_product = goodProd()
    #similarity_data_frame_ref = pd.DataFrame(list(col_item_item_matrix.find()))
    similarity_data_frame = pd.DataFrame(list(col_item_item_matrix.find()))
    similarity_data_frame = similarity_data_frame.loc[similarity_data_frame.loc[:,'index'].isin(unique_good_product),similarity_data_frame.columns[0:165]]
    similarity_data_frame = similarity_data_frame[similarity_data_frame.loc[:,:]>0]
    similarity_data_frame = similarity_data_frame[similarity_data_frame.loc[:,:]<1]
    similarity_data_frame = similarity_data_frame.fillna(0)
    #avgSimilarity = (similarity_data_frame.mean()).mean()
    
    to_recomend_list = list([])
    
    for kk in similarity_data_frame.columns:
        positive_similarity = similarity_data_frame.loc[(similarity_data_frame.loc[:,kk]>0)==True,kk]
        positive_similarity_median = np.median(positive_similarity)
        positive_similarity = positive_similarity[positive_similarity >= positive_similarity_median]
        positive_similarity = positive_similarity[positive_similarity<0.99999999]
        
        to_recomend_list.extend(positive_similarity.index)
        
    to_recomend_list = set(to_recomend_list)
    to_recomend_list = list(map(int, to_recomend_list))
    return(to_recomend_list)
    

# content-based - to calculate precision & recall
results = content_based()
oscores = [(i,s) for x,v in results.items() for s,i in v if s != 0]
mean_oscore = np.mean([y for x,y in oscores])
rproducts = [(pid,oscore) for pid,oscore in oscores if oscore>mean_oscore]
content_to_recommend = countProductIDs(rproducts).keys()

### Model Evaluation
# In[5]
#Content Based

content_prediction, content_recall = calculate_metrics(content_to_recommend)

#Item BAsed
item_to_recommend = jp_eval_all()
item_prediction, item_recall = calculate_metrics(item_to_recommend)

# hybrid - to calculate precision & recall
hybrid_prediction, hybrid_recall = calculate_hybrid_metrics(content_to_recommend,item_to_recommend)

# In[6]
ItemBased,ContentBased,HybridBased = call_recomndation(User_Id,Prod_id,Knear)

print("-" * 64)
print("Recomendation : Item Based")
print("-"*32)
print(ItemBased)
print("Content based : Precision : %.5f , Recall : %.5f \n" % (content_prediction,content_recall))

print("-" * 64)
print("Recomendation : Content Based")
print("-"*32)
print(ContentBased)
print("Item based : Precision : %.5f , Recall : %.5f \n" % (item_prediction,item_recall))


print("-" * 64)
print("Recomendation : Hybrid Based [Weighted : ContentBased(0.7),Collaborative(0.3)]")
print("-"*32)
print(HybridBased)
print("hybrid based : Precision : %.5f , Recall : %.5f \n" % (hybrid_prediction,hybrid_recall))
