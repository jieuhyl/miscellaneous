# -*- coding: utf-8 -*-
"""
Created on Tue Aug 27 11:04:20 2019

@author: Jie.Hu
"""



import warnings
warnings.filterwarnings('ignore') 
import numpy as np
import pandas as pd
import seaborn as sns


# read data
df = pd.read_csv('corr_0827.csv')

# check missing again
df.isnull().values.sum()

df.isnull().sum()/df.shape[0]

df = df.dropna()
#df['Country']= df['Country'].astype(str)
# change the value
#df['Country'].value_counts()

df.columns.tolist()

# get t0
df = df[df['Window'] ==0]

# two studios
df = df[df['Distributor'].isin(['WARNER','PARAMOUNT'])]
df = df[df['Country'].isin(['AUS', 'RUS'])]

df = df[(df['Country'].isin(['ESP', 'MEX', 'RUS'])) & df['Distributor'].isin(['PARAMOUNT'])]

sns.set_style("whitegrid")
sns.lmplot(x='DefinitePurchaseInterest', y='OpeningBO', hue='Country', data=df)
sns.lmplot(x='DefinitePurchaseInterest', y='OpeningBO', hue='Country',  fit_reg=False, data=df)
sns.lmplot(x='DefinitePurchaseInterest', y='OpeningBO', hue='Country', col='Country', data=df)


# overall x genre
df_corr0 = pd.DataFrame(df.groupby(['Country', 'Genre'])[['OpeningBO','UnaidedAwareness','TotalAwareness','DefinitePurchaseInterest','InterestAll','FirstChoiceAll','UnaidedIntent']].corr().iloc[0::7][['UnaidedAwareness','TotalAwareness','DefinitePurchaseInterest','InterestAll','FirstChoiceAll','UnaidedIntent']]).reset_index()
df_corr0.drop('level_2', axis = 1, inplace=True)
df_corr0.dropna(thresh = 6, inplace=True)
df_corr0.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0827/Country_Genre_v2.csv', index=False) 

# overall x distributor
df_corr0 = pd.DataFrame(df.groupby(['Country', 'Distributor'])[['OpeningBO','UnaidedAwareness','TotalAwareness','DefinitePurchaseInterest','InterestAll','FirstChoiceAll','UnaidedIntent']].corr().iloc[0::7][['UnaidedAwareness','TotalAwareness','DefinitePurchaseInterest','InterestAll','FirstChoiceAll','UnaidedIntent']]).reset_index()
df_corr0.drop('level_2', axis = 1, inplace=True)
#df_corr0.dropna(how='all', inplace=True)
df_corr0.dropna(thresh = 6, inplace=True)
df_corr0.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0827/Country_Distributor_v2.csv', index=False)   




df_corr0 = pd.DataFrame(df.groupby(['Country', 'Distributor'])[['OpeningBO','UnaidedAwareness','TotalAwareness','DefinitePurchaseInterest','InterestAll','FirstChoiceAll','UnaidedIntent']].corr())
