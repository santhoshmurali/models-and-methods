import pandas as pd
import pandasql as pdsql
import numpy as nm
import datetime

klachaks_Data = pd.read_excel(".\Data\Overall2.xls", index_col= None)
df_Klachaks = pd.DataFrame(klachaks_Data)

i = 0
rowNamesWithBadStartDate = []

#Cleaning of Start Date and End Date


for st in df_Klachaks['Start']:
    if type(st)!= datetime.datetime:
        df_Klachaks.loc[[i],'Start'] = nm.nan
        print(df_Klachaks.iloc[i, 0], " | Date Value = ", st,"***-***", df_Klachaks.loc[[i],'Start'])
    i+=1


qryString = "select Name from df_Klachaks where Rental < 1000 limit 3;"
pysql = lambda q: pdsql.sqldf(q,globals())
df1 = pysql(qryString)
