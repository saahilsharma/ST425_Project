# Data Manipulation
import numpy as np
import pandas as pd

# Visualization
import matplotlib.pyplot as plt
import seaborn as sns
plt.style.use('seaborn-whitegrid')

# Preprocessing
from sklearn.preprocessing import MinMaxScaler
import copy

# Machine learning
from sklearn.model_selection import train_test_split
from sklearn.svm import LinearSVC
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import precision_score, recall_score, f1_score #for model evaluation
from sklearn.metrics import confusion_matrix #for model evaluation
from sklearn.model_selection import cross_val_score
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
from sklearn.svm import SVC
from sklearn.model_selection import cross_val_predict

def plot_confusion_matrix(cm, title):
    ax= plt.subplot()
    sns.heatmap(cm, annot=True, ax = ax)
    ax.set_xlabel('Predicted labels'),ax.set_ylabel('True labels')
    ax.set_title('Confusion Matrix ' + title);
    ax.xaxis.set_ticklabels(['Did not die', 'Died']), ax.yaxis.set_ticklabels(['Did not die', 'Died'])
    plt.show()


def fit_ml_algo(algo, name, X_train, Y_train, X_test, Y_test, cv, storage):
    """ Focus, on precision score, recall score and F1 score (balance between precision and recall)"""
    storage['Name'].append(name)
    # TRAIN
    train_pred = cross_val_predict(algo, X_train, Y_train, cv=cv)
    cm_train = confusion_matrix(Y_train, train_pred)  # train confusion matrix
    f1_train = round(f1_score(Y_train, train_pred) * 100, 2)
    prec_train = round(precision_score(Y_train, train_pred) * 100, 2)
    rec_train = round(recall_score(Y_train, train_pred) * 100, 2)

    # TEST
    model = algo.fit(X_train, Y_train)
    algo_pred = model.predict(X_test)
    cm_test = confusion_matrix(Y_test, algo_pred)  # test confusion matrix
    f1_test = round(f1_score(Y_test, algo_pred) * 100, 2)
    prec_test = round(precision_score(Y_test, algo_pred) * 100, 2)
    rec_test = round(recall_score(Y_test, algo_pred) * 100, 2)

    storage['Train F1'].append(f1_train)
    storage['Test F1'].append(f1_test)
    storage['Train precision'].append(prec_train)
    storage['Test precision'].append(prec_test)
    storage['Train recall'].append(rec_train)
    storage['Test recall'].append(rec_test)

    plot_confusion_matrix(cm_test, name)

def find_optimized_tuining_param(C_range):
    f1_score_train = []
    rec_score_train = []
    prec_score_train = []

    for c in C_range:
        svc = SVC(kernel='linear', C=c, class_weight='balanced')
        scores_f1 = cross_val_score(svc, X_train, Y_train, cv=10, scoring='f1')
        scores_rec = cross_val_score(svc, X_train, Y_train, cv=10, scoring='recall')
        scores_prec = cross_val_score(svc, X_train, Y_train, cv=10, scoring='precision')
        f1_score_train.append(scores_f1.mean())
        rec_score_train.append(scores_rec.mean())
        prec_score_train.append(scores_prec.mean())

    # find the C which gives the highest training F1
    max_f1 = max(f1_score_train)
    C_f1_index = f1_score_train.index(max_f1)
    C_f1 = C_range[C_f1_index]
    return C_f1


### Start of Script ###
heart_failure_data = pd.read_csv("Heart Failure.csv")

#MinMax Scaling as we're working with boolean variables
scaler = MinMaxScaler()
scaler.fit(heart_failure_data)
df_scaled = pd.DataFrame(scaler.transform(heart_failure_data))
col = heart_failure_data.columns
df_scaled.columns = col

Y = df_scaled['DEATH_EVENT']
X = df_scaled.drop(['DEATH_EVENT'], axis = 1)

#split the data into training and testing set
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.4, random_state=42)

#  Dataframe to store results of each method
storage = {'Train F1' : [], 'Train precision': [], 'Train recall' : [], 'Test F1': [], 'Test precision' :[],
          'Test recall' : [], 'Name' : []}


fit_ml_algo(LogisticRegression(class_weight = 'balanced'), 'LR', X_train, Y_train, X_test, Y_test, 10, storage)
fit_ml_algo(LinearDiscriminantAnalysis(), 'LDA', X_train, Y_train, X_test, Y_test, 10, storage)
fit_ml_algo(SVC(kernel='linear', class_weight = 'balanced'), 'SVM Linear with C = 1 (Default)', X_train, Y_train, X_test, Y_test, 10, storage)

C_f1 = find_optimized_tuining_param(list(range(1, 40)))
fit_ml_algo(SVC(kernel='linear', C = C_f1, class_weight = 'balanced'), "SVM Linear with C = " + str(C_f1), X_train, Y_train, X_test, Y_test, 10, storage)

dataframe = pd.DataFrame(storage, index = storage['Name'])
dataframe.drop(['Name'], axis = 1, inplace = True)
dataframe.index.name = 'Name'
pd.set_option("display.max_rows", None, "display.max_columns", None)
print(dataframe)