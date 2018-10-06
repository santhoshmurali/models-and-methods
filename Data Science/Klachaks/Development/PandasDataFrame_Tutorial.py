import pandas as pd
import pandasql as pdsql
import numpy as nm
import datetime

klachaks_Data = pd.read_excel(".\Data\Overall2.xls", index_col= None)
df_Klachaks = pd.DataFrame(klachaks_Data)
qryString = "select Name from df_Klachaks where Rental < 1000 limit 3;"
pysql = lambda q: pdsql.sqldf(q,globals())
df1 = pysql(qryString)
print(df1)


i = 0
rowNamesWithBadStartDate = []

for st in df_Klachaks['Start']:
    if type(st)!= datetime.datetime:
        print(df_Klachaks.iloc[i,0], "Date Value = ", st, " Row Number =",i)
        df_Klachaks.iloc[]
    i+=1




