# -*- coding: utf-8 -*-
"""
Created on Fri Sep 06 14:19:36 2019

@author: Jie.Hu
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style("whitegrid")


df = pd.read_csv('Nrglc_CT_CUME.csv')


# check missing
df.isnull().values.sum()
missing_ratio = df.isnull().sum() / len(df)
missing_ratio.sort_values(ascending=True)[10:]


df1 = df[['MVID', 'TITLE', 'MEASURE', 'TOTAL']]

df2 = df1.pivot_table(index =['MVID'],
                columns = ['MEASURE'],
                values = ['TOTAL'])
df2.columns = df2.columns.map(lambda x: x[1])
df2 = df2.reset_index() 

df3 = df.drop_duplicates(subset = ['MVID'])
df4 = pd.merge(df2, df3, how = 'left', on='MVID')
df4.drop(['MEASURE', 'TOTAL'], axis = 1, inplace = True)

#
df4.isnull().values.sum()
missing_ratio = df4.isnull().sum() / len(df4)

#df = df.dropna()
df4 = df4[df['MVID'] != 71562]
df4 = df4.loc[:, df4.isnull().mean() < 0.2]
df4 = df4.fillna(df4.median())


# correlations
df4['MVID'] = df4['MVID'].astype(str)
df4['T_WINDOW'] = df4['T_WINDOW'].astype(str)
df4['SEQUEL'] = df4['SEQUEL'].astype(str)
df4['SEQ'] = df4['SEQ'].astype(str)


corr = df4.corr()
plt.subplots(figsize=(30, 30))
cmap = sns.diverging_palette(150, 250, as_cmap=True)
sns.heatmap(corr, cmap="RdYlBu", vmax=1, vmin=-0.6, center=0.2, square=True, linewidths=0, cbar_kws={"shrink": .5}, annot = False)

k = 20 #number of variables for heatmap
cols = corr.nlargest(k, 'WKND_OPENING_BOX_OFFICE')['WKND_OPENING_BOX_OFFICE'].index
cm = np.corrcoef(df4[cols].values.T)
plt.subplots(figsize=(30, 30))
sns.heatmap(cm, cmap="RdYlGn", cbar=True, annot=True, square=True, fmt='.2f', annot_kws={'size': 10}, yticklabels=cols.values, xticklabels=cols.values)

corr.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0906/corr_table.csv', index=True)
df4.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0906/df4.csv', index=False)



#  Feature Selection
df4.columns.get_loc('Very unique and original') 
drop_features = ['MVID', 'TITLE', 'WDATE', 'REPORTDATE', 'RELEASE_DATE', 'T_WINDOW','SCREENS',
                 'TOTAL_GRS', 'GENRE', 'RATING', 'BASEDON', 'SEQUEL', 'DISTRIBUTOR',
                 'CRITICSCORE', 'AUDIENCESCORE', 'SEQ', 'AUDIENCE','WKND_OPENING_BOX_OFFICE']
df5 = df4.drop(drop_features, axis = 1)

X = df5.values
y = df4['WKND_OPENING_BOX_OFFICE'].values

# transformation
from sklearn.preprocessing import MinMaxScaler, StandardScaler
ss = StandardScaler()
#mm = MinMaxScaler()
X = ss.fit_transform(X)

# feature extraction
'''
# KBEST
from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import f_regression

test = SelectKBest(score_func=f_regression, k=10)
fit = test.fit(X, y)
# summarize scores
print(fit.scores_)
feature_imp = pd.DataFrame({'Features': df5.columns.tolist(),
                                'Importance': fit.scores_})

# TREE
from sklearn.ensemble import ExtraTreesRegressor
# feature extraction
model = ExtraTreesRegressor()
model.fit(X, y)
print(model.feature_importances_)

feature_imp = pd.DataFrame({'Features': df5.columns.tolist(),
                            'Importance': model.feature_importances_})

'''

# RFE
from sklearn.feature_selection import RFE, RFECV
from sklearn.linear_model import LinearRegression
#from sklearn.ensemble import RandomForestRegressor

# feature extraction
model = LinearRegression()
#model = RandomForestRegressor()
rfe = RFE(model, 10, step=1) 
#rfe = RFECV(model, step=1, cv=5)
fit = rfe.fit(X, y)

print("Num Features: %d") % fit.n_features_
print("Selected Features: %s") % fit.support_
print("Feature Ranking: %s") % fit.ranking_

feature_imp = pd.DataFrame({'Features': df5.columns.tolist(),
                            'Select': fit.support_,
                            'Rank':fit.ranking_})

feature_imp.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0906/feature_imp_rf.csv', index=False)


#### CM data

df = pd.read_csv('Nrglc_CM_CUME.csv')


# check missing
df.isnull().values.sum()
missing_ratio = df.isnull().sum() / len(df)
missing_ratio.sort_values(ascending=True)[10:]


df1 = df[['MVID', 'TITLE', 'MEASURE', 'TOTAL']]

df2 = df1.pivot_table(index =['MVID'],
                columns = ['MEASURE'],
                values = ['TOTAL'])
df2.columns = df2.columns.map(lambda x: x[1])
df2 = df2.reset_index() 

df3 = df.drop_duplicates(subset = ['MVID'])
df4 = pd.merge(df2, df3, how = 'left', on='MVID')
df4.drop(['MEASURE', 'TOTAL'], axis = 1, inplace = True)

#
df4.isnull().values.sum()
missing_ratio = df4.isnull().sum() / len(df4)

#df = df.dropna()
df4 = df4.loc[:, df4.isnull().mean() < 0.2]
df4 = df4.fillna(df4.median())


# correlations
df4['MVID'] = df4['MVID'].astype(str)
df4['T_WINDOW'] = df4['T_WINDOW'].astype(str)
df4['SEQUEL'] = df4['SEQUEL'].astype(str)
df4['SEQ'] = df4['SEQ'].astype(str)


corr = df4.corr()
plt.subplots(figsize=(30, 30))
cmap = sns.diverging_palette(150, 250, as_cmap=True)
sns.heatmap(corr, cmap="RdYlBu", vmax=1, vmin=-0.6, center=0.2, square=True, linewidths=0, cbar_kws={"shrink": .5}, annot = False)

k = 20 #number of variables for heatmap
cols = corr.nlargest(k, 'WKND_OPENING_BOX_OFFICE')['WKND_OPENING_BOX_OFFICE'].index
cm = np.corrcoef(df4[cols].values.T)
plt.subplots(figsize=(30, 30))
sns.heatmap(cm, cmap="RdYlGn", cbar=True, annot=True, square=True, fmt='.2f', annot_kws={'size': 10}, yticklabels=cols.values, xticklabels=cols.values)

corr.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0906/corr_table_CM.csv', index=True)
df4.to_csv('C:/Users/Jie.Hu/Desktop/Correlation/corr_0906/df4_CM.csv', index=False)


