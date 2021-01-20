# -*- coding: utf-8 -*-
"""
Created on Thu Jan 14 17:30:10 2021

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


# to binary
def mapping(x):
    if x==1:
        val = 1
    else:
        val = 0
    return val
   
df.iloc[:,1:] = df.iloc[:,1:].applymap(mapping)



''' model '''
# get mtx
X = df.iloc[:, 2:].values
y = df.iloc[:,1].values


''' Logistic Regression'''
from sklearn.metrics import roc_auc_score, f1_score, accuracy_score, classification_report
from sklearn.model_selection import StratifiedKFold, cross_val_score, GridSearchCV
from sklearn.linear_model import LogisticRegression


clf_lr = LogisticRegression(penalty = 'l1', solver = 'saga', random_state = 1337)
clf_lr.fit(X, y)

# Predicting the train set results
y_pred = clf_lr.predict(X)
roc_auc_score(y, y_pred)

#skf = StratifiedKFold(n_splits=5)
acc = cross_val_score(estimator = clf_lr, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()


# KF n GS
parameters = {'C': [0.001, 0.01, 0.1, 1, 10, 100, 1000]}

grid_search = GridSearchCV(estimator = clf_lr,
                           param_grid = parameters,
                           scoring='accuracy',
                           cv = 3,
                           n_jobs = -1)

grid_search = grid_search.fit(X, y)
grid_search.best_params_, grid_search.best_score_

# last step
clf_lr = grid_search.best_estimator_
#clf_lr = LogisticRegression(penalty = 'l1', solver = 'saga', C = 0.1, random_state = 1337)

clf_lr.fit(X, y)
y_pred = clf_lr.predict(X)

acc = cross_val_score(estimator = clf_lr, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()

print(classification_report(y, y_pred))

sub = pd.DataFrame({"Attribute": df.iloc[:, 2:].columns, 
                    "Coefficient": clf_lr.coef_[0,:],
                    "Impact Index": (np.exp(clf_lr.coef_[0,:])/(1+np.exp(clf_lr.coef_[0,:]))*2)*100})


# merge to get label
#df_label = pd.read_csv('label.csv')
#sub = pd.merge(sub, df_label, left_on=['Attribute'], right_on=['Attribute'], how='left')

sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0114/DA_outputs_lr_l1_norm.csv', index=False)


clf_lr = LogisticRegression(penalty='none')
clf_lr.fit(X, y)

odds_ratio=np.exp(clf_lr.coef_)
sub = pd.DataFrame({"Attribute": df.iloc[:, 2:].columns, 
                    "Coefficient": clf_lr.coef_[0,:],
                    "odds_ratio": odds_ratio,
                    "Impact Index": (np.exp(clf_lr.coef_[0,:])/(1+np.exp(clf_lr.coef_[0,:]))*2)*100})

    
    
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

clf_nb.fit(X, y)
y_pred = clf_nb.predict(X)

acc = cross_val_score(estimator = clf_nb, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()

print(classification_report(y, y_pred))

# outputs
df['QPOSTINT'].value_counts(normalize = True)
df[['QPOSTINT', 'QGENREr1', 'QGENREr2', 'QAGREEMENTr12']].groupby(['QPOSTINT']).agg(['mean', 'count'])
pd.crosstab(df['QAGREEMENTr12'], df['QPOSTINT'])

output = np.exp(clf_nb.class_log_prior_)

prior_pos = output[1]
prior_neg = output[0]

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
    
sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0114/DA_outputs_nb.csv', index=False)   





''' Random Forest '''
from sklearn.ensemble import RandomForestClassifier

clf_rf = RandomForestClassifier(random_state = 1337)
clf_rf.fit(X, y)

acc = cross_val_score(estimator = clf_rf, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()


# KF n GS
parameters = {'criterion':['gini', 'entropy'],
              'max_depth':[3,5,7],
              'max_features':[0.3,0.5,0.7],
              'max_samples':[0.3,0.5,0.7],
              'n_estimators':[50, 100]}
                                       
grid_search = GridSearchCV(estimator = clf_rf,
                           param_grid = parameters,
                           scoring='accuracy',
                           cv = 3,
                           n_jobs = -1)

grid_search = grid_search.fit(X, y)
grid_search.best_params_, grid_search.best_score_


# last step
clf_rf = grid_search.best_estimator_

clf_rf.fit(X, y)
y_pred = clf_rf.predict(X)

acc = cross_val_score(estimator = clf_rf, X = X, y = y, cv = 3, scoring='accuracy')
acc.mean(), acc.std()

print(classification_report(y, y_pred))


# feature importance
fi = clf_rf.feature_importances_
predictors = [x for x in df.iloc[:, 2:].columns]
feat_imp = pd.Series(fi, predictors).sort_values(ascending=False)
feat_imp.plot(kind='bar', title='Feature Importances')
plt.ylabel('Feature Importance Score')

sub = pd.DataFrame({"Attribute": df.iloc[:, 2:].columns, 
                    "Coefficient": clf_rf.feature_importances_,
                    "Impact Index": clf_rf.feature_importances_/(np.mean(clf_rf.feature_importances_))*100}).sort_values(by='Impact Index', ascending=False)

sub.to_csv('C:/Users/Jie.Hu/Desktop/Driver Analysis/0114/DA_outputs_rf.csv', index=False) 