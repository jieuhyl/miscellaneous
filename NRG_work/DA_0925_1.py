# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 15:49:47 2019

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
df = pd.read_csv('da_0925_1.csv', skipinitialspace=True)

# check missing again
df.isnull().values.sum()
df.isnull().sum()/df.shape[0]
df.columns.tolist()

# overall  
#df = df.iloc[:,0:82]
#df = df.dropna()
# impute missing with 0  
#df.fillna(0, inplace = True)

df['QPOSTINT'] = df['QPOSTINT'].apply(lambda x: 1 if x==1 else 0)
df['QCOMICFAMILIAR'] = df['QCOMICFAMILIAR'].apply(lambda x: 1 if x==1 else 0)
df[['QCOMICFAMILIAR', 'QPOSTINT']].groupby(['QCOMICFAMILIAR'], as_index=False).mean().sort_values(by='QPOSTINT', ascending=False)


# mapping
#  gender and parent
df['QGENDER'] = df['QGENDER'].map({1:1,2:0})
# 
t_dummies  = pd.get_dummies(df['QAUD2'], prefix='QAUD2')
df = df.join(t_dummies)
df.drop(['QAUD2'], axis=1, inplace=True)

t_dummies  = pd.get_dummies(df['QPEDIGREE'], prefix='QPEDIGREE')
df = df.join(t_dummies)
df.drop(['QPEDIGREE'], axis=1, inplace=True)


# top box, responses
df.columns.get_loc('QPOSTINT') 
df.columns.get_loc('QPEDIGREE_4') 

def mapping(x):
    if x==1:
        val = 1
    else:
        val = 0
    return val
   
df.iloc[:,2:63] = df.iloc[:,2:63].applymap(mapping)


# get mtx
# POSTINT
# overall
X = df.iloc[:,3:].values
y = df.iloc[:,2].values

# sub
df_sub = df[df['QCELLCODE'] == 1]
X = df_sub.iloc[:,3:].values
y = df_sub.iloc[:,2].values


from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score
from sklearn.metrics import confusion_matrix
from sklearn import metrics

mod_rf = RandomForestClassifier(random_state=1337)
mod_rf.fit(X, y)
y_pred = mod_rf.predict(X)
cm = confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)

'''
# k fold and grid
parameters = {
              'n_estimators':[30,50,70,90,110],
              'max_depth':[3,5,7],
              'max_features':['auto','log2', 'sqrt']
              }
                                       
grid_search = GridSearchCV(estimator = mod_rf,
                           param_grid = parameters,
                           scoring= 'accuracy',
                           cv = 3,
                           n_jobs = -1)
grid_search = grid_search.fit(X, y)
grid_search.best_score_,  grid_search.best_params_


# last step
mod_rf = RandomForestClassifier(n_estimators=50,
                                max_depth=7,
                                max_features='log2',
                                random_state=1337)
mod_rf.fit(X, y)
y_pred = mod_rf.predict(X)
cm = confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)
'''

# feature importance
fi = mod_rf.feature_importances_
predictors = [x for x in df.iloc[:,3:].columns]
feat_imp = pd.Series(mod_rf.feature_importances_, predictors).sort_values(ascending=False)
feat_imp.plot(kind='bar', title='Feature Importances')
plt.ylabel('Feature Importance Score')

#top 30 attributes
#pickup = feat_imp[:30].index.tolist()
#pickup.remove('QTVGENREr20')
pickup = feat_imp[feat_imp > 1.0/len(feat_imp)].index

#drops = ['QTVWATCH2r1','QTVWATCH2r21','QTVWATCH2r33','QTVWATCH2r35','QTVWATCH2r8',
#         'QAud2_2', 'QAud2_3','QGENDER']
#pickup = list(set(pickup) - set(drops))
#df_new['pcntcharsinvested'] = df['pcntcharsinvested']
pickup = list(set(pickup))
pickup.extend(['QCOMICFAMILIAR'])


X = df_sub.loc[:, pickup].values


from sklearn.linear_model import LogisticRegression
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


sub = pd.DataFrame({"Attribute": df.loc[:, pickup].columns, 
                    "Coefficient": mod_lr.coef_[0,:],
                    "Impact Index": (np.exp(mod_lr.coef_[0,:])/(1+np.exp(mod_lr.coef_[0,:]))*2)*100})


# merge to get label
#df_label = pd.read_csv('label.csv')
#sub = pd.merge(sub, df_label, left_on=['Attribute'], right_on=['Attribute'], how='left')

sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0925/DA_outputs_1_CELL1_v2.csv', index=False)



df_sub[['QCOMICFAMILIAR', 'QPOSTINT']].groupby(['QCOMICFAMILIAR'], as_index=False).mean().sort_values(by='QPOSTINT', ascending=False)



#==============================================================================
''' RFE '''
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

rfe = RFECV(mod_lr, min_features_to_select= 20, step = 1, cv = 5)
#rfe = RFE(mod_lr, 20, step = 1)
fit = rfe.fit(X, y)

print("Num Features: %d") % fit.n_features_
print("Selected Features: %s") % fit.support_
print("Feature Ranking: %s") % fit.ranking_

feature_imp = pd.DataFrame({'Features': df_sub.iloc[:,3:].columns.tolist(),
                            'Select': fit.support_,
                            'Rank':fit.ranking_})

#feature_imp.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0925/feature_imp.csv', index=False)

#pickup = feature_imp[feature_imp['Rank']==1]['Features']
pickup  = df_sub.iloc[:,3:].columns[fit.support_]
#drops = ['QTVWATCH2r1','QTVWATCH2r21','QTVWATCH2r33','QTVWATCH2r35','QTVWATCH2r8',
#         'QAud2_2', 'QAud2_3','QGENDER']
#pickup = list(set(pickup) - set(drops))

#pickup = list(set(pickup))
#pickup.extend(['QTVWATCHr14','QAUD2_1','QAUD2_3','QAUD2_4','QAUD2_2'])

X = df_sub.loc[:, pickup].values

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


sub = pd.DataFrame({"Attribute": df.loc[:, pickup].columns, 
                    "Coefficient": mod_lr.coef_[0,:],
                    "Impact Index": (np.exp(mod_lr.coef_[0,:])/(1+np.exp(mod_lr.coef_[0,:]))*2)*100}).sort_values(by='Impact Index', ascending=False)

sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0925/DA_outputs_1_CELL1_v4.csv', index=False)


# =============================================================================
''' regularization lr '''
from sklearn.linear_model import LogisticRegression
mod_lr = LogisticRegression()
mod_lr.fit(X, y)
y_pred = mod_lr.predict(X)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
from sklearn import  metrics
confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)

# k fold and grid
parameters = {
              'penalty':['l2'],
              'C':[0.001,0.01,0.1,1,10,100,1000],
              'solver':['saga', 'liblinear', 'newton-cg', 'lbfgs']
              }

parameters = {
              'penalty':['l1'],
              'C':[0.001,0.01,0.1,1,10,100,1000],
              'solver':['saga', 'liblinear']
              }
                                       
grid_search = GridSearchCV(estimator = mod_lr,
                           param_grid = parameters,
                           scoring= 'accuracy',
                           cv = 3,
                           n_jobs = -1)
grid_search = grid_search.fit(X, y)
grid_search.best_score_,  grid_search.best_params_


# last step
mod_lr = LogisticRegression(penalty = 'l1', C=0.5, solver = 'saga')
#mod_lr = LogisticRegression(penalty = 'l2', C=10, solver = 'newton-cg')
mod_lr.fit(X, y)
y_pred = mod_lr.predict(X)

# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
from sklearn import  metrics
confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)


sub = pd.DataFrame({"Attribute": df_sub.iloc[:,3:].columns, 
                    "Coefficient": mod_lr.coef_[0,:],
                    "Impact Index": (np.exp(mod_lr.coef_[0,:])/(1+np.exp(mod_lr.coef_[0,:]))*2)*100})
