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

#Preprocessing
# Loading transactions data
path_to_data_excel = argv[1] 
#raw_data = pd.read_excel("C:\Home\Work\GreatLakes\capstone\Package\latestproducts_with_id.xlsx")
raw_data = pd.read_excel(path_to_data_excel)
products = raw_data.groupby(['ProductID','ProductName']).size().to_frame().reset_index().drop(0,axis=1)
products.head()

#PreProcessing for Scoring and Metrics
client = MongoClient('localhost',27017)
database = client.RE
col_item_user_rating_train = database.train_item_user_rating
col_item_user_rating_test = database.test_item_user_rating
col_item_item_matrix = database.item_item_matrix
User_Id = int(argv[2])
Prod_id = int(argv[3])
Knear = int(argv[4])
rec = int(argv[5])




#Recommendations
# In[1]
# ITEM BASED RECOMMENDATION
# -----------------------------
def item_recommend(n_customer,n_product,n_similarities,types):
    temp_similarity_df = pd.DataFrame()
    l1 = list(database['item_item_matrix'].find({},{"_id":0,str(n_product):1,"index":1}).sort(str(n_product),pymongo.DESCENDING).limit(n_similarities).skip(1))
    temp_similarity_df= pd.DataFrame(l1)
    similarity_Array = (temp_similarity_df['index'])
    similarity_Array = list(similarity_Array.values)
    recomended_Products =  list(database[types+'_item_user_rating'].find({"$and":[{"customer":n_customer},{"product":{"$in":similarity_Array}},{"predicted":"Y"}]},{"_id":0,"product":1,"rating":1}).sort('rating',pymongo.DESCENDING))
    recomended_Products_df = pd.DataFrame(recomended_Products)
    df_similarity_score = cp.deepcopy(temp_similarity_df)
    df_similarity_score.columns = ['Similarity', 'product']
    df_similarity_score['product'] = df_similarity_score['product'].astype('Int64')
    recomendation_df = pd.merge(recomended_Products_df, df_similarity_score, on='product', how='left')
    recomendation_df['UserId'] = n_customer
    recomendation_df = recomendation_df.loc[:,['UserId','product','Similarity','rating']]
    recomendation_df.columns = ['UserID','ProductID','Similarity','Ratings']
    return(recomendation_df.T.to_dict())
    
# In[2]
# CONTENT BASED RECOMMENDATION
# -----------------------------
def content_based():
    tf = TfidfVectorizer(analyzer='word', ngram_range=(1, 3), min_df=0, stop_words='english')
    tfidf_matrix = tf.fit_transform(products['ProductName'])

    cosine_similarities = linear_kernel(tfidf_matrix, tfidf_matrix)

    results = {}

    for idx, row in products.iterrows():
        similar_indices = cosine_similarities[idx].argsort()[:-100:-1]
        similar_items = [(cosine_similarities[idx][i], products['ProductID'][i]) for i in similar_indices]

        # First item is the item itself, so remove it.
        # Each dictionary entry is like: [(1,2), (3,4)], with each tuple being (score, item_id)
        results[row['ProductID']] = sorted(list(set(similar_items)),key= lambda x: x[0],reverse=True)[1:]
    return results

def recommend(prod_id, num):
    results = content_based()
    recs = {y:x for x,y in results[prod_id][:num]}
    return recs

# In[3]
# HYBRID BASED RECOMMENDATION
# -----------------------------
def hybrid(user_id,prod_id,num,types):
    '''Getting content based recommendations - productid,scores'''
    content_weight = 0.7
    content_scores = recommend(prod_id,num)
    #content_scores = {88: 0.082614172251057924, 90: 0.084871240138180593, 131: 0.10800215179039482}
    cscores = [(pid,score*content_weight) for pid,score in content_scores.items()]
        
    '''Getting item based recommendations - only productid and scores as tuples which are not in content scores''' 
    item_scores = item_recommend(user_id,prod_id,num,types) 
    item_weight = 0.3
    scores = [(v['ProductID'],v['Similarity'] * item_weight) for i,v in item_scores.items() if v['ProductID'] not in content_scores.keys()]
    
    scores.extend(cscores)
    return dict(sorted(scores,key=lambda x: x[1],reverse=True)[:num])

# In[Calling Recommendation]
def call_recomndation(uid,pid,k,types):
    # Item Based
    ib  = pd.DataFrame(item_recommend(uid,pid,k,types))
    ib = ib.T
    ib = ib.loc[:,['ProductID','Similarity']]
    
    # Content Based
    cb_op = recommend(pid,k)
    cb = pd.DataFrame.from_dict(cb_op,orient='index')
    cb = cb.reset_index()
    cb.columns = ['ProductID','Similarity']
    
    # Hybrid 
    hb_op = hybrid(uid,pid,k,types)
    hb = pd.DataFrame.from_dict(hb_op,orient='index')
    hb = hb.reset_index()
    hb.columns = ['ProductID','Similarity']    
   
    return(ib,cb,hb)

### Functions for Model Evaluation
# In[4]
# Returning Good Product
def goodProd(types,thresh):
    all_prd = pd.DataFrame(list(col_item_user_rating_train.find())) if types == 'train' else pd.DataFrame(list(col_item_user_rating_test.find()))
        
    good_prd = all_prd.loc[all_prd.loc[:,'predicted']=='N',:]
    unique_good_product = np.unique(good_prd['product'])
   # to_recomend = all_prd.loc[all_prd.loc[:,'predicted']=='Y',:]
    good_prod = set(good_prd.loc[good_prd.loc[:,'rating']>thresh,'product'])
    return(good_prod,unique_good_product)

# Number of Products
def countProductIDs(product_scores):
    count = Counter(i[0] for i in product_scores)
    return count

# Hits - Product that are good and recomended
def calculate_hits(predictions, types, thresh):
    goodProdS = goodProd(types, thresh)
    good_ids = goodProdS[0]
    
    hits = 0

    for id in predictions:
        for gid in good_ids:
            if(int(id) == gid):
                hits += 1

    return hits,len(good_ids)

# Calucalte Precision and Recall
def calculate_metrics(pred_to_recommend,types, thresh):
    hits,goodCount = calculate_hits(pred_to_recommend,types, thresh)

    precision = hits/len(pred_to_recommend)
    recall = hits/goodCount

    return precision,recall

# Calculate Precision and Recall for Hybrid
def calculate_hybrid_metrics(content_to_recommend,item_to_recommend, types, thresh):
    hybrid_to_recommend = [id for id in content_to_recommend if id in item_to_recommend]

    hits,good_count = calculate_hits(hybrid_to_recommend,types, thresh)
    
    precision = hits/len(hybrid_to_recommend)
    recall = hits/good_count
    
    return precision,recall

# Item Based Evaluation
def jp_eval_all(types):
    good_prod,unique_good_product = goodProd(types,3)
    #similarity_data_frame_ref = pd.DataFrame(list(col_item_item_matrix.find()))
    similarity_data_frame = pd.DataFrame(list(col_item_item_matrix.find()))
    similarity_data_frame = similarity_data_frame.loc[similarity_data_frame.loc[:,'index'].isin(unique_good_product),similarity_data_frame.columns[0:165]]
    
    avgSimilarity = (similarity_data_frame.mean()).mean()
    
    to_recomend_list = list([])
    
    for k in range(len(similarity_data_frame.index)):
        
        #print("k = %d and mean = %.3f" % (k,temp_avg))
        temp_avg_non_0 = 0
        non_zero_cnt = 1
        ss = 0
        for kk in np.nan_to_num(similarity_data_frame.iloc[:,k]):
            ss = ss + kk
            if (kk!=0.0):
                non_zero_cnt+=1
        temp_avg_non_0 = ss/non_zero_cnt
        temp_avg_non_0 = temp_avg_non_0 if temp_avg_non_0 == 0.0 else avgSimilarity
        
        g = 0
        for idx in similarity_data_frame.iloc[:,k]:
            if idx > temp_avg_non_0 and idx != 1 and idx !=0 :
                #print("value= %.3f and idx = %d" % (idx,g))
                to_recomend_list.append(similarity_data_frame.columns[g])
            g+=1
            
    to_recomend_list = set(to_recomend_list)
    to_recomend_list = list(map(int, to_recomend_list))
    return(to_recomend_list)
    

# content-based - to calculate precision & recall
results = content_based()
oscores = [(i,s) for x,v in results.items() for s,i in v if s != 0]
mean_oscore = np.mean([y for x,y in oscores])
rproducts = [(pid,oscore) for pid,oscore in oscores if oscore>mean_oscore]


### Model Evaluation
# In[5]
#Content Based

content_to_recommend = countProductIDs(rproducts).keys()
train_content_prediction, train_content_recall = calculate_metrics(content_to_recommend,'train',3)
test_content_prediction, test_content_recall = calculate_metrics(content_to_recommend,'test',3)

#Item BAsed
train_item_to_recommend = jp_eval_all('train')
test_item_to_recommend = jp_eval_all('test')
train_item_prediction, train_item_recall = calculate_metrics(train_item_to_recommend,'train',3)
test_item_prediction, test_item_recall = calculate_metrics(test_item_to_recommend,'test',3)

# hybrid - to calculate precision & recall
train_hybrid_prediction, train_hybrid_recall = calculate_hybrid_metrics(content_to_recommend,train_item_to_recommend,'train',3)
test_hybrid_prediction, test_hybrid_recall = calculate_hybrid_metrics(content_to_recommend,test_item_to_recommend,'test',3)

# In[6]
Train_ItemBased,Train_ContentBased,Train_HybridBased = call_recomndation(User_Id,Prod_id,Knear,'train')
Test_ItemBased,Test_ContentBased,Test_HybridBased = call_recomndation(User_Id,Prod_id,Knear,'test')

def recomend_print(outPutRecomend):
    if(outPutRecomend==0):
        print("-" * 64)
        print("Recomendation : Item Based")
        print("-"*32)
        print(Test_ItemBased)
        print("Train Item based : Precision : %.5f , Recall : %.5f \n" % (train_item_prediction,train_item_recall))
        print("Test Item based : Precision : %.5f , Recall : %.5f \n" % (test_item_prediction,test_item_recall))
        
        print("-" * 64)
        print("Recomendation : Content Based")
        print("-"*32)
        print(Test_ContentBased)
        print("Train Content based : Precision : %.5f , Recall : %.5f \n" % (train_content_prediction,train_content_recall))
        print("Test Content based : Precision : %.5f , Recall : %.5f \n" % (test_content_prediction,test_content_recall))
        
        
        print("-" * 64)
        print("Recomendation : Hybrid Based [Weighted : ContentBased(0.7),Collaborative(0.3)]")
        print("-"*32)
        print(Test_HybridBased)
        print("Train hybrid based : Precision : %.5f , Recall : %.5f \n" % (train_hybrid_prediction,train_hybrid_recall))
        print("Test hybrid based : Precision : %.5f , Recall : %.5f \n" % (test_hybrid_prediction,test_hybrid_recall))
    else:
        return(Train_ItemBased)


outPutProd = recomend_print(1)
outPutProd.to_csv(path_or_buf="c:\\Temp\\RecOp.csv",index_label="index")
