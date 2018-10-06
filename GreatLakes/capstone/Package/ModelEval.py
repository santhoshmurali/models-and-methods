
# coding: utf-8
#python ModelEval.py

# In[1]:

from pymongo import MongoClient
import numpy as np
import pandas as pd


# In[2]:
client = MongoClient('localhost',27017)
database = client.RE

# In[5]:
col_item_user_rating = database.item_user_rating
col_item_item_matrix = database.item_item_matrix

# In[44]
#precision_df = recommend(User_Id,Prod_id,Knear)
#temp = precision_df.loc[:,['UserID','ProductSelected','Similarity']].groupby(['UserID','ProductSelected']).mean()
# In[56]
def goodProd():
    all_prd = pd.DataFrame(list(col_item_user_rating.find()))
    good_prd = all_prd.loc[all_prd.loc[:,'predicted']=='N',:]
    unique_good_product = np.unique(good_prd['product'])
   # to_recomend = all_prd.loc[all_prd.loc[:,'predicted']=='Y',:]
    good_prod = set(good_prd.loc[good_prd.loc[:,'rating']>3,'product'])
    return(good_prod,unique_good_product)
# In[45] : JP Eval
def jp_eval():
    good_prod,unique_good_product = goodProd()
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
    
    hits = 0
    
    for y in to_recomend_list:
        for z in good_prod:
            if (y==z):
                hits+=1

    
    precision = hits/len(to_recomend_list)
    recall = hits/len(good_prod)
    
    return(precision,recall,hits,len(to_recomend_list),len(good_prod))
#good_products_rated = len(np.unique(precision_df[0]))
#films_to_recomend = len(np.unique(precision_df[1]))
evaluate = jp_eval()
print("precision = %.9f" % evaluate[0])
print("recall = %.9f" % evaluate[1])
print("hits = %.0f" % evaluate[2])
print("to_recomend = %.0f" % evaluate[3])
print("good = %.f" % evaluate[4])


