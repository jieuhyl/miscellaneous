# -*- coding: utf-8 -*-
"""
Created on Thu Dec 05 06:59:46 2019

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


# read data
df = pd.read_csv('da_1204_v2.csv', skipinitialspace=True)

# check missing again
df.isnull().values.sum()
df.isnull().sum()/df.shape[0]
missing_ratio = df.isnull().sum() / len(df)
df.columns.tolist()


# swap income and education
df['QINCOME_DE'].fillna(df['QINCOME_US'], inplace=True)
df['QEDUCATION_DE'].fillna(df['QEDUCATION_US'], inplace=True)
df.drop(['QINCOME_US', 'QEDUCATION_US'], axis = 1, inplace=True)
df.rename(columns={'QINCOME_DE': 'QINCOME', 'QEDUCATION_DE':'QEDUCATION'}, inplace=True)

# age
def f(row):
    if row['QAGE'] <= 19:
        val = "age_1619"
    elif row['QAGE'] > 20 and row['QAGE'] <= 24:
        val = "age_2024"
    elif row['QAGE'] > 25 and row['QAGE'] <= 34:
        val = "age_2534"
    elif row['QAGE'] > 35 and row['QAGE'] <= 44:
        val = "age_3544"
    else:
        val = "age_4554"
    return val

df['QAGE'] = df.apply(f, axis=1)

# fill with "i dont know"
df = df.fillna(5)

#
t_dummies  = pd.get_dummies(df['QAGE'], prefix='QAGE')
df = df.join(t_dummies)
df.drop(['QAGE'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QRELIGION'], prefix='QRELIGION')
df = df.join(t_dummies)
df.drop(['QRELIGION'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QRELIGIOUSLEVEL'], prefix='QRELIGIOUSLEVEL')
df = df.join(t_dummies)
df.drop(['QRELIGIOUSLEVEL'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QORTHODOXFAMILIARITY'], prefix='QORTHODOXFAMILIARITY')
df = df.join(t_dummies)
df.drop(['QORTHODOXFAMILIARITY'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QINCOME'], prefix='QINCOME')
df = df.join(t_dummies)
df.drop(['QINCOME'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QEDUCATION'], prefix='QEDUCATION')
df = df.join(t_dummies)
df.drop(['QEDUCATION'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QARTHOUSEFILMS'], prefix='QARTHOUSEFILMS')
df = df.join(t_dummies)
df.drop(['QARTHOUSEFILMS'], axis=1, inplace=True)

df['QGENDER'] = df['QGENDER'].map({1:1,2:0})


df.columns.get_loc('QGENREr1') 
df.columns.get_loc('QGENREr7')
def mapping(x):
    if x==10:
        val = 1
    else:
        val = 0
    return val
   
df.iloc[:,9:16] = df.iloc[:,9:16].applymap(mapping)


df['QINTENT'].value_counts()
df['QINTENT'] = df['QINTENT'].map({1:1,2:1,3:0,4:0,5:0})


''' model '''
# get mtx
# us
df_us = df[df['country'] == 'us']
df_us.columns.tolist()
df_us.columns.get_loc('QGENREr7') 
df_us.columns.get_loc('QNETWORKVIEW_USr1')
df_us.columns.get_loc('QNETWORKVIEW_USr18')
df_us.columns.get_loc('QSHOWVIEW_USr1')
X = df_us.iloc[:, np.r_[3:15, 39:57, 94:187]].values
y = df_us.iloc[:,2].values

# de
df_de = df[df['country'] == 'de']
df_de.drop(['QETHNICITYr1',
            'QETHNICITYr2',
            'QETHNICITYr3',
            'QETHNICITYr4',
            'QETHNICITYr5'], axis = 1, inplace=True)
df_de.columns.tolist()
df_de.columns.get_loc('QNETWORKVIEW_DEr23') 
df_de.columns.get_loc('QSHOWVIEW_DEr1') 
df_de.columns.get_loc('QSHOWVIEW_DEr37') 
df_de.columns.get_loc('QAGE_age_1619')  
X = df_de.iloc[:, np.r_[3:34, 52:89,134:182]].values
y = df_de.iloc[:,2].values



from sklearn.feature_selection import RFE, RFECV
from sklearn.linear_model import LogisticRegression
mod_lr = LogisticRegression()
mod_lr.fit(X, y)
y_pred = mod_lr.predict(X)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
from sklearn import  metrics
confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)


#rfe = RFECV(mod_lr, min_features_to_select= 25, step = 1, cv = 5)
rfe = RFE(mod_lr, 25, step = 1)
fit = rfe.fit(X, y)

print("Num Features: %d") % fit.n_features_
print("Selected Features: %s") % fit.support_
print("Feature Ranking: %s") % fit.ranking_

feature_imp = pd.DataFrame({'Features': df_us.iloc[:, np.r_[3:15, 39:57, 94:187]].columns.tolist(),
                            'Select': fit.support_,
                            'Rank':fit.ranking_}).sort_values(by='Rank', ascending=True)

feature_imp.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/1204/DA_feature_imp_us_v2.csv', index=False)

#pickup = feature_imp[feature_imp['Rank']==1]['Features']
pickup  = df_us.iloc[:, np.r_[3:15, 39:57, 94:187]].columns[fit.support_]

# drop
#drops = ['PRG_QRACE_2', 'PRG_QRACE_4']
#pickup = list(set(pickup) - set(drops))

# add new
pickup = list(set(pickup))
pickup.extend(['QRELIGIOUSLEVEL_1.0',
 'QRELIGIOUSLEVEL_2.0',
 'QRELIGIOUSLEVEL_3.0',
 'QRELIGIOUSLEVEL_4.0',
 'QRELIGIOUSLEVEL_5.0',
 'QORTHODOXFAMILIARITY_1',
 'QORTHODOXFAMILIARITY_2',
 'QORTHODOXFAMILIARITY_3',
 'QORTHODOXFAMILIARITY_4',
 'QORTHODOXFAMILIARITY_5'])
pickup = list(set(pickup))    

X = df_us.loc[:, pickup].values

mod_lr = LogisticRegression()
mod_lr.fit(X, y)
y_pred = mod_lr.predict(X)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
from sklearn import  metrics
confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)

# Applying k-Fold Cross Validation
from sklearn.model_selection import cross_val_score
accuracies = cross_val_score(estimator = mod_lr, X = X, y = y, cv = 5)
accuracies.mean(), accuracies.std() 


sub = pd.DataFrame({"Attribute": df_us.loc[:, pickup].columns, 
                    "Coefficient": mod_lr.coef_[0,:],
                    "Impact Index": (np.exp(mod_lr.coef_[0,:])/(1+np.exp(mod_lr.coef_[0,:]))*2)*100}).sort_values(by='Impact Index', ascending=False)

sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/1204/DA_outputs_us_v3.csv', index=False)
