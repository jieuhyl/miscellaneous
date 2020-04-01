# -*- coding: utf-8 -*-
"""
Created on Tue Mar 31 16:45:53 2020

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
df = pd.read_csv('us04388online-mall-final.csv', skipinitialspace=True)

# check missing again
df.isnull().values.sum()
df.isnull().sum()/df.shape[0]
missing_ratio = df.isnull().sum() / len(df)
df.columns.tolist()

#genre 1
dff = df.iloc[:, np.r_[470,565:585]]
dff.columns = [i.replace('Lr1', '') for i in dff.columns.tolist()]
dff = dff.dropna()

#genre 3
dff = df.iloc[:, np.r_[472,723:743]]
dff.columns = [i.replace('Lr2', '') for i in dff.columns.tolist()]
dff = dff.dropna()

#genre 4
dff = df.iloc[:, np.r_[473,883:903]]
dff.columns = [i.replace('Lr3', '') for i in dff.columns.tolist()]
dff = dff.dropna()

#genre 5
dff = df.iloc[:, np.r_[474,1043:1063]]
dff.columns = [i.replace('Lr4', '') for i in dff.columns.tolist()]
dff = dff.dropna()


# 
def mapping(x):
    if x==1:
        val = 1
    else:
        val = 0
    return val
   
dff.iloc[:,0:1] = dff.iloc[:,0:1].applymap(mapping)

dff['QFANSHIPr1'].value_counts(normalize = True)
dff['QFANSHIPr3'].value_counts(normalize = True)
dff['QFANSHIPr4'].value_counts(normalize = True)
dff['QFANSHIPr5'].value_counts(normalize = True)



''' model '''
# get mtx
X = dff.iloc[:, 1:].values
y = dff.iloc[:,0].values


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
rfe = RFE(mod_lr, 10, step = 1)
fit = rfe.fit(X, y)

print("Num Attribute: %d") % fit.n_features_
print("Selected Attribute: %s") % fit.support_
print("Feature Ranking: %s") % fit.ranking_

feature_imp = pd.DataFrame({'Attribute': dff.iloc[:, 1:].columns.tolist(),
                            'Select': fit.support_,
                            'Rank':fit.ranking_}).sort_values(by='Rank', ascending=True)

# merge to get label
df_label = pd.read_csv('label.csv')
df_label['Attribute'] = [i.replace('Lr1', '') for i in df_label['Attribute']]
sub = pd.merge(feature_imp, df_label, left_on=['Attribute'], right_on=['Attribute'], how='left')
sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0331/DA_feature_imp_genre1.csv', index=False)


pickup = feature_imp[feature_imp['Rank']==1]['Attribute']
#pickup  = df_us.iloc[:, np.r_[3:15, 39:57, 94:187]].columns[fit.support_]
   

X = dff.loc[:, pickup].values

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


sub = pd.DataFrame({"Attribute": dff.loc[:, pickup].columns, 
                    "Coefficient": mod_lr.coef_[0,:],
                    "Impact Index": (np.exp(mod_lr.coef_[0,:])/(1+np.exp(mod_lr.coef_[0,:]))*2)*100}).sort_values(by='Impact Index', ascending=False)

# merge to get label
df_label = pd.read_csv('label.csv')
df_label['Attribute'] = [i.replace('Lr1', '') for i in df_label['Attribute']]
sub = pd.merge(sub, df_label, left_on=['Attribute'], right_on=['Attribute'], how='left')
sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0331/DA_outputs_genre1.csv', index=False)