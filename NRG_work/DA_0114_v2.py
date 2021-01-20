# -*- coding: utf-8 -*-
"""
Created on Tue Jan 19 17:51:52 2021

@author: Jie.Hu
"""



import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_style("whitegrid")
import warnings
warnings.filterwarnings('ignore') 



# read data
df = pd.read_csv('da_0114.csv', skipinitialspace=True)

# check missing again
df.isnull().values.sum()
df.isnull().sum()/df.shape[0]
missing_ratio = df.isnull().sum() / len(df)
df.columns.tolist()


# massacre
df['QMASSACRE'] = np.where(df['QMASSACRE'] >= 4, 1, 0)

# d.v.
df['QPOSTINT'] = df['QPOSTINT'].apply(lambda x: 1 if x == 1 else 0)

# to binary top2 box
def mapping(x):
    if x == 1: #or x == 1:
        val = 1
    else:
        val = 0
    return val
   
df.iloc[:,2:] = df.iloc[:,2:].applymap(mapping)


''' model '''
# get mtx
X = df.iloc[:, 2:].values
y = df.iloc[:,1].values


    
''' Naive Bayes '''
from sklearn.metrics import roc_auc_score, f1_score, accuracy_score, classification_report
from sklearn.model_selection import StratifiedKFold, cross_val_score, GridSearchCV
from sklearn.naive_bayes import BernoulliNB, MultinomialNB, GaussianNB


clf_nb = BernoulliNB()
clf_nb.fit(X, y)


acc = cross_val_score(estimator = clf_nb, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()



# KF n GS
parameters = {'alpha': [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]}

grid_search = GridSearchCV(estimator = clf_nb,
                           param_grid = parameters,
                           scoring='accuracy',
                           cv = 3,
                           n_jobs = -1)

grid_search = grid_search.fit(X, y)
grid_search.best_params_, grid_search.best_score_

# last step
clf_nb = grid_search.best_estimator_
#params = {'alpha': 1}
#clf_nb.set_params(**params)


clf_nb.fit(X, y)
y_pred = clf_nb.predict(X)

acc = cross_val_score(estimator = clf_nb, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()

print(classification_report(y, y_pred))

# outputs
df['QPOSTINT'].value_counts(normalize = True)
df[['QPOSTINT', 'QGENREr1', 'QATTRIBUTESr3', 'QAGREEMENTr12']].groupby(['QPOSTINT']).agg(['mean', 'count'])
pd.crosstab(df['QAGREEMENTr12'], df['QPOSTINT'])


output = np.exp(clf_nb.class_log_prior_)

prior_pos = output[1]
prior_neg = output[0]

#prior_pos = 0.5
#prior_neg = 0.5

log_prob_pos = clf_nb.feature_log_prob_[1]
log_prob_neg = clf_nb.feature_log_prob_[0]

prob_pos = np.exp(clf_nb.feature_log_prob_[1])
prob_neg = np.exp(clf_nb.feature_log_prob_[0])

odds_pos = (prior_neg/prior_pos) * (clf_nb.feature_log_prob_[0]/clf_nb.feature_log_prob_[1])
odds_neg = (prior_pos/prior_neg) * (clf_nb.feature_log_prob_[1]/clf_nb.feature_log_prob_[0])



sub = pd.DataFrame({"Attribute": df.iloc[:, 2:].columns, 
                    "log_Prob_Positive": log_prob_pos,
                    "log_Prob_Negative": log_prob_neg,
                    "Prob_Positive": prob_pos,
                    "Prob_Negative": prob_neg,
                    "Odds_Positive": odds_pos,
                    "Odds_Negative": odds_neg,
                    })
    
sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0114/DA_outputs_nb_v2.csv', index=False)   