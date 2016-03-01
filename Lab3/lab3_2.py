import pandas as pd
from sklearn import linear_model
from sklearn import lda
from sklearn import neighbors
from sklearn import qda
import numpy as np

__author__ = 'randyjp'

# (a) Load the data using the give csv file.
default = pd.read_csv("Default.csv")
# (b) Extract two predictors, income and balance, to use for building the model.
default1 = default[['default', 'income', 'balance']]
# (c) Split the first 8,000 records into the training set and the remaining as the testing set.
train = default1[0:8000]
test = default1[8000:10000]
# (d) Build a logistic regression model and report both the training and testing accuracies.
logistic = linear_model.LogisticRegression()
logistic.fit(train[['income', 'balance']], train['default'])
print("Logistic regression train accuracy: " + str(
    np.mean(logistic.predict(train[['income', 'balance']]) == train['default'])))  # train accuracy
print("Logistic regression test accuracy: " + str(
    np.mean(logistic.predict(test[['income', 'balance']]) == test['default'])))  # test accuracy

# (e) Build a linear discriminant analysis model and report both the training and testing accuracies.
ldamodel = lda.LDA(solver="svd")
ldamodel.fit(train[['income', 'balance']], train['default'])
print("LDA train accuracy: " + str(np.mean(ldamodel.predict(train[["income", "balance"]]) == train['default'])))
print("LDA test accuracy: " + str(np.mean(ldamodel.predict(test[["income", "balance"]]) == test['default'])))

# (f) Build a k-nearest neighbor model and report both the training and testing
# accuracies. You also need to experiment with different k values.
# k = 1
k = 5
# k = 10
#k = 50
knn = neighbors.KNeighborsClassifier(k)
knn.fit(train[['income', 'balance']], train['default'])
print("knn with k=" + str(k) + " train accuracy: " + str(
    np.mean(knn.predict(train[["income", "balance"]]) == train['default'])))
print("knn with k=" + str(k) + " test accuracy: " + str(
    np.mean(knn.predict(test[["income", "balance"]]) == test['default'])))

# (g) Build a quadratic discriminant analysis model and report both the training and
# testing accuracies.
qdamodel = qda.QDA()
qdamodel.fit(train[['income', 'balance']], train['default'])
print("QDA train accuracy: " + str(np.mean(qdamodel.predict(train[["income", "balance"]]) == train['default'])))
print("QDA test accuracy: " + str(np.mean(qdamodel.predict(test[["income", "balance"]]) == test['default'])))


# (h) Think strategies to improve the test accuracy.
# This would be change the threshold that represents the "Yes" in default. lowering it to 0.3 instead of
# 0.5 would help to improve the test accuracy because we are more interested in the ones that might default
# than obtaining the highest accuracy
