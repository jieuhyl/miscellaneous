# -*- coding: utf-8 -*-
"""
Created on Wed Sep 25 16:06:29 2019

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
df = pd.read_csv('da_0925_2.csv', skipinitialspace=True)

# check missing again
df.isnull().values.sum()
df.isnull().sum()/df.shape[0]
df.columns.tolist()

# overall  
#df = df.dropna()
# impute missing with 0  
#df.fillna(0, inplace = True)

# mapping
df['QPOSTINT'] = df['QPOSTINT'].apply(lambda x: 1 if x==1 else 0)

# 
t_dummies  = pd.get_dummies(df['QMYSTERIES'], prefix='QMYSTERIES')
df = df.join(t_dummies)
df.drop(['QMYSTERIES'], axis=1, inplace=True)


# get mtx
# POSTINT
# overall
X = df.iloc[:,3:].values
y = df.iloc[:,2].values

# sub
df_sub = df[df['QCELLCODE'] == 3]
X = df_sub.iloc[:,3:].values
y = df_sub.iloc[:,2].values


from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score
from sklearn.metrics import confusion_matrix
from sklearn import metrics

mod_rf = RandomForestClassifier(n_estimators=70,
                                max_depth=5,
                                max_features=None,
                                random_state=1337)
mod_rf.fit(X, y)
y_pred = mod_rf.predict(X)
cm = confusion_matrix(y, y_pred)
metrics.accuracy_score(y, y_pred)


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
#pickup1.append('pcntcharsinvested')


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

sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0925/DA_outputs_2_cell_3.csv', index=False)