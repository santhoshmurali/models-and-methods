# -*- coding: utf-8 -*-
"""
Created on Thu Dec  7 10:51:53 2017

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

raw_data = pd.read_excel("C:\Home\Work\GreatLakes\capstone\Package\latestproducts_with_id.xlsx")
products = raw_data.groupby(['ProductID','ProductName']).size().to_frame().reset_index().drop(0,axis=1)
products.head()

client = MongoClient('localhost',27017)
database = client.RE
col_item_user_rating = database.item_user_rating
col_item_item_matrix = database.item_item_matrix

item_user_matrix = pd.DataFrame(list(col_item_user_rating.find()))
item_user_matrix = item_user_matrix.loc[:,['customer','product','rating','predicted']]
item_user_matrix.loc[item_user_matrix.loc[:,'rating']==0,'rating']=1
item_user_matrix_original = item_user_matrix.loc[item_user_matrix.loc[:,'predicted']=='N',['customer','product','rating']]
item_user_matrix_predicted = item_user_matrix.loc[item_user_matrix.loc[:,'predicted']=='Y',['customer','product','rating']]
item_item_matrix = pd.DataFrame(list(col_item_item_matrix.find()))

item_item_matrix_sim = item_item_matrix[((item_item_matrix.loc[:,:]>0) & (item_item_matrix.loc[:,:]<1))].fillna(0)
item_user_matrix['hits']=0
item_user_matrix['n_good_item']=0
item_user_matrix['n_to_recomend']=0

def rated_product_count(cid):
    item_user_matrix_original_count = item_user_matrix_original.loc[item_user_matrix_original.loc[:,'customer']==cid,['customer','product']].groupby(['customer']).count()
    item_user_matrix_original_count = item_user_matrix_original_count.reset_index()
    len(item_user_matrix_original_count.index)
    num_of_prod = item_user_matrix_original_count['product'][0] if len(item_user_matrix_original_count.index) > 0 else 1 
    return(num_of_prod)
    
def good_prod_list(cid,threshold):
    return(list(item_user_matrix_original.loc[((item_user_matrix_original.loc[:,'customer']==cid) & (item_user_matrix_original.loc[:,'rating']>threshold)),'product']))


def to_recomend_product(pid,threshold):
    pid = str(pid)
    threshold = len(item_item_matrix_sim)-1 if threshold==0 else threshold
    temp_r = item_item_matrix_sim.loc[:,pid].sort_values(0, ascending=False)[:threshold]
    temp_r = pd.DataFrame(temp_r)
    temp_r = temp_r.reset_index()
    temp_r.columns = ['product','Similarity']
    return(temp_r, list(temp_r['product']), list(temp_r['Similarity']))
    
                    
def to_find_hits(cid,pid,rThreshold, sThreshold):
    gp_list = good_prod_list(cid,rThreshold)
    tr_list = to_recomend_product(pid,sThreshold)[1]
    hits = 0
    for gp in gp_list:
        for tr in tr_list:
            if(gp==tr):
                hits+=1
    return(hits)


for ium in range(len(item_user_matrix)):
    customer_id = item_user_matrix.iloc[ium,0]
    product_id= item_user_matrix.iloc[ium,1]
    #print(customer_id)
    item_user_matrix.iloc[ium,4] =  to_find_hits(customer_id,product_id,2,30)
    item_user_matrix.iloc[ium,5] = rated_product_count(customer_id)
    item_user_matrix.iloc[ium,6] = 30



