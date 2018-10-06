
# coding: utf-8

import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel
from sys import argv
import numpy as np
from collections import Counter
import pymongo
from pymongo import MongoClient
import copy as cp


# Loading transactions data
path_to_data_excel = argv[1] 
raw_data = pd.read_excel(path_to_data_excel)
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


# Returning Good Product
def goodProd():
    all_prd = pd.DataFrame(list(col_item_user_rating.find()))
    good_prd = all_prd.loc[all_prd.loc[:,'predicted']=='N',:]
    unique_good_product = np.unique(good_prd['product'])
   # to_recomend = all_prd.loc[all_prd.loc[:,'predicted']=='Y',:]
    good_prod = set(good_prd.loc[good_prd.loc[:,'rating']>3,'product'])
    return(good_prod,unique_good_product)

def countProductIDs(product_scores):
    count = Counter(i[0] for i in product_scores)
    return count

def calculate_hits(predictions):
    goodProdS = goodProd()
    good_ids = goodProdS[0]
    
    hits = 0
    precision = 0
    recall = 0

    for id in predictions:
        hit = good_ids[good_ids['product'] == int(id)]
        
        if len(hit) == 1:
            hits += 1

    return hits,len(good_ids)


def calculate_metrics(pred_to_recommend):
    hits,goodCount = calculate_hits(pred_to_recommend)

    precision = hits/len(pred_to_recommend)
    recall = hits/goodCount

    return precision,recall

def calculate_hybrid_metrics(content_to_recommend,item_to_recommend):
    hybrid_to_recommend = [id for id in content_to_recommend if str(id) in item_to_recommend]

    hits,good_count = calculate_hits(hybrid_to_recommend)
    
    precision = hits/len(hybrid_to_recommend)
    recall = hits/good_count
    
    return precision,recall

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

def recommend(prod_id, num=5):
    results = content_based()
    recs = {y:x for x,y in results[prod_id][:num]}
    return recs

# content-based - to calculate precision & recall
results = content_based()
oscores = [(i,s) for x,v in results.items() for s,i in v if s != 0]
mean_oscore = np.mean([y for x,y in oscores])
rproducts = [(pid,oscore) for pid,oscore in oscores if oscore>mean_oscore]
content_to_recommend = countProductIDs(rproducts).keys()
content_prediction, content_recall = calculate_metrics(content_to_recommend)

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


# item-based - to calculate precision & recall
item_to_recommend = ['1','10','100','101','102','103','104','105','107','108','109','11','110','111','112','113','114','115',
                         '116','117','118','119','12','120','121','122','123','124','125','126','128','129','13','130','131','132',
                         '133','134','135','136','137','138','139','14','140','141','142','143','144','145','146','147','148','149',
                         '15','150','151','152','153','154','155','156','157','158','159','16','160','161','162','163','164','165',
                         '166','17','19','2','20','21','22','23','24','25','26','28','29','3','30','31','32','33','34','35','36','37',
                         '38','39','4','40','41','42','43','44','45','46','47','48','49','5','50','51','52','53','54','55','56','57',
                         '58','59','6','60','61','62','63','64','65','66','67','68','7','70','71','72','73','74','75','76','77','78',
                         '79','8','81','82','83','84','85','87','88','89','90','91','92','93','94','95','96','97','98']
item_prediction, item_recall = calculate_metrics(item_to_recommend)

# HYBRID BASED RECOMMENDATION
# -----------------------------
def hybrid(user_id,prod_id,num):
    '''Getting content based recommendations - productid,scores'''
    content_weight = 0.7
    content_scores = recommend(prod_id,num)
    #content_scores = {88: 0.082614172251057924, 90: 0.084871240138180593, 131: 0.10800215179039482}
    cscores = [(pid,score*content_weight) for pid,score in content_scores.items()]
        
    '''Getting item based recommendations - only productid and scores as tuples which are not in content scores''' 
    item_scores = item_recommend(user_id,prod_id,num) 
    item_weight = 0.3
    scores = [(v['ProductID'],v['Similarity'] * item_weight) for i,v in item_scores.items() if v['ProductID'] not in content_scores.keys()]
    
    scores.extend(cscores)
    return dict(sorted(scores,key=lambda x: x[1],reverse=True)[:num])

# hybrid - to calculate precision & recall
hybrid_prediction, hybrid_recall = calculate_hybrid_metrics(content_to_recommend,item_to_recommend)


print('Content recommendations:')
print('*' * 26)
print(recommend(Prod_id,Knear))
print('-' * 30)
print('Precision: {}'.format(content_prediction))
print('Recall: {}'.format(content_recall))
print('-' * 30)
print('\n')
print('Hybrid recommendations:')
print('*' * 26)
print(hybrid(User_Id,Prod_id,Knear))
print('-' * 30)
print('Precision: {}'.format(hybrid_prediction))
print('Recall: {}'.format(hybrid_recall))
print('-' * 30)
print('\n')
print('Item recommendations:')
print('*' * 26)
print(item_recommend(User_Id,Prod_id,Knear))
print('-' * 30)