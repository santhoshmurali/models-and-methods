
# coding: utf-8
#python HybridSystem.py <path of xlsx> User_Id Prod_id Knear

import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import linear_kernel
from sys import argv
#Santhosh
import pymongo
from pymongo import MongoClient
import copy as cp

path_to_data_excel = argv[1] 
raw_data = pd.read_excel(path_to_data_excel)
#raw_data = pd.read_csv(path_to_data_csv)
products = raw_data.groupby(['ProductID','ProductName']).size().to_frame().reset_index().drop(0,axis=1)
products.head()

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

def recommend(prod_id, num):
    recs = {y:x for x,y in results[prod_id][:num]}
    return recs
        
# recommend(prod_id=11, num=5) # check content recommendation


#Item Based
#Setting-Up pyMongo
client = MongoClient('localhost',27017)
database = client.RE
col_item_user_rating = database.item_user_rating
col_item_item_matrix = database.item_item_matrix

User_Id = int(argv[2])
Prod_id = int(argv[3])
Knear = int(argv[4])

def cf_recommend(n_customer,n_product,n_similarities):
    #n_customer = 3 #int(argument[1])
    #n_product = 11 #int(argument[2])
    #n_similarities = 5 #int(argument[3])
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


#def item_recommend(user_id,pnum):
#    d = {0: {'UserID':25.0,'ProductID':13.0,'Similarity':0.243470259233764912,'Ratings':2.0},
#        1: {'UserID':25.0,'ProductID':35.0,'Similarity':0.1945936918682043,'Ratings':2.0},
#        2: {'UserID':25.0,'ProductID':36.0,'Similarity':0.2224293292787618,'Ratings':2.0},
#        3: {'UserID':25.0,'ProductID':55.0,'Similarity':0.23289988977162202,'Ratings':2.0},
#        4: {'UserID':25.0,'ProductID':66.0,'Similarity':0.22677868380553634,'Ratings':2.0},}
#    return d

def hybrid(user_id,prod_id,num):
    '''Getting content based recommendations - productid,scores'''
    content_weight = 0.7
    content_scores = recommend(prod_id,num)
    #content_scores = {88: 0.082614172251057924, 90: 0.084871240138180593, 131: 0.10800215179039482}
    cscores = [(pid,score*content_weight) for pid,score in content_scores.items()]
        
    '''Getting item based recommendations - only productid and scores as tuples which are not in content scores''' 
    item_scores = cf_recommend(user_id,prod_id,num) 
    item_weight = 0.3
    scores = [(v['ProductID'],v['Similarity'] * item_weight) for i,v in item_scores.items() if v['ProductID'] not in content_scores.keys()]
    
    scores.extend(cscores)
    return dict(sorted(scores,key=lambda x: x[1],reverse=True))
    
print(hybrid(User_Id,Prod_id,Knear))

