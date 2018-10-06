
# coding: utf-8

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

# In[43]:
def evaluation():
    all_prd = pd.DataFrame(list(col_item_user_rating.find()))
    good_prd = all_prd.loc[all_prd.loc[:,'predicted']=='N',:]
    to_recomend = all_prd.loc[all_prd.loc[:,'predicted']=='Y',:]
    
    unique_good_product = np.unique(good_prd['product'])
    
    similarity_data_frame = pd.DataFrame(list(col_item_item_matrix.find()))
    similarity_data_frame = similarity_data_frame.loc[similarity_data_frame.loc[:,'index'].isin(unique_good_product),similarity_data_frame.columns[0:166]]
    #avgSimilarity = (similarity_data_frame.mean()).mean()
    avgSimilaritybyProduct = pd.DataFrame(similarity_data_frame.mean())
    avgSimilaritybyProduct = avgSimilaritybyProduct.reset_index()
    avgSimilaritybyProduct.columns = ['product','similarity']
    avgSimilaritybyProduct['product'] = avgSimilaritybyProduct['product'].astype('Int64')
    
    good_prd = pd.merge(good_prd,avgSimilaritybyProduct, on='product', how='left')
    good_prd = good_prd.loc[:,['customer','product','rating','similarity']]
    goodPrdAvg = good_prd['similarity'].mean()
    
    to_recomend = pd.merge(to_recomend,avgSimilaritybyProduct, on='product', how='left')
    to_recomend = to_recomend.loc[:,['customer','product','rating','similarity']]
    toRecAvg = to_recomend['similarity'].mean()
    
    good_prod_hit = good_prd.loc[good_prd.loc[:,'similarity']>goodPrdAvg,:]
    to_recomend_hit = to_recomend.loc[to_recomend.loc[:,'similarity']>toRecAvg,:]
    
    precision = len(to_recomend_hit)/len(to_recomend)
    recall = len(good_prod_hit)/len(good_prd)
    return(precision,recall)

# In[44]
#precision_df = recommend(User_Id,Prod_id,Knear)
#temp = precision_df.loc[:,['UserID','ProductSelected','Similarity']].groupby(['UserID','ProductSelected']).mean()

#good_products_rated = len(np.unique(precision_df[0]))
#films_to_recomend = len(np.unique(precision_df[1]))
evaluate = evaluation()
print("precision = %.2f" % evaluate[0] + "\n")
print("recall = %.2f" % evaluate[1])


