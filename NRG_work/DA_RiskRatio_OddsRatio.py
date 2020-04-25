# -*- coding: utf-8 -*-
"""
Created on Fri Apr 24 15:58:02 2020

@author: Jie.Hu
"""



import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style("whitegrid")
import warnings
warnings.filterwarnings('ignore') 
from scipy import stats
from scipy.stats import norm, skew 
import time


# read data
df = pd.read_csv('da_0420.csv', skipinitialspace=True)

# check missing again
df.isnull().values.sum()
df.isnull().sum()/df.shape[0]
missing_ratio = df.isnull().sum() / len(df)
df.columns.tolist()


# 
df['QPOSTINT'].value_counts(normalize = True)
df['QPOSTINT'] = df['QPOSTINT'].apply(lambda x: 1 if x==1 else 0)

def mapping(x):
    if x==1:
        val = 1
    else:
        val = 0
    return val
   
df.iloc[:,2:] = df.iloc[:,2:].applymap(mapping)


df[['QPOSTINT','Engaging host(s) ']].groupby(['QPOSTINT']).agg(['mean', 'count'])



def rror(cols):
    outcome = cols.columns.tolist()[0]
    expo = cols.columns.tolist()[1]
    
    a = len(cols[(cols[outcome] == 1) & (cols[expo] == 1)])
    b = len(cols[(cols[outcome] == 0) & (cols[expo] == 1)])
    c = len(cols[(cols[outcome] == 1) & (cols[expo] == 0)])
    d = len(cols[(cols[outcome] == 0) & (cols[expo] == 0)])
    
    RR = (a/(a+b))/(c/(c+d))
    OR = (a*d)/(b*c)
    return (RR, OR)

rror(df[['QPOSTINT', 'Engaging host(s) ']])



def rror(cols):
    lst = []
    for col in range(1, cols.shape[1]):
        outcome = cols.columns.tolist()[0]
        expo = cols.columns.tolist()[col]
        
        a = len(cols[(cols[outcome] == 1) & (cols[expo] == 1)])
        b = len(cols[(cols[outcome] == 0) & (cols[expo] == 1)])
        c = len(cols[(cols[outcome] == 1) & (cols[expo] == 0)])
        d = len(cols[(cols[outcome] == 0) & (cols[expo] == 0)])
        
        RR = (a/(a+b))/(c/(c+d))
        OR = (a*d)/(b*c)
        lst.append((RR, OR))
    return lst

lst = rror(df.iloc[:,1:])

sub = pd.DataFrame({"Attribute": df.iloc[:,2:].columns, 
                    "RR": [x[0] for x in lst],
                    "RR Index": [100*x[0] for x in lst]/np.mean([x[0] for x in lst]),
                    "OR": [x[1] for x in lst],
                    "OR Index": [100*x[1] for x in lst]/np.mean([x[1] for x in lst]),
                    "Impact Index": ([100*x[0] for x in lst]/np.mean([x[0] for x in lst])+[100*x[1] for x in lst]/np.mean([x[1] for x in lst]))/2}).sort_values(by='Impact Index', ascending=False)
    
sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0420/DA_outputs_rror.csv', index=False)  
 
    
    
    
    
    
    

