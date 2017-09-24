#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
R Template for HACKtheMACHINE September 22nd and 23rd
Created on Sun Sep 17 13:55:52 2017

@author: danielbrownjr
"""

# Import Packages
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Import package modules
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import Imputer
from sklearn.preprocessing import StandardScaler
from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LinearRegression

# Import dataset
dataset = pd.read_csv('dataset.csv')
x = dataset.iloc[:, 1:-1].values
y = dataset.iloc[:, 4].values

# Taking care of missing data
imputer = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0)
imputer = imputer.fit(x[:, 1:4])
x[:, 1:4] = imputer.transform(x[:, 1:4])

# Encoding categorical data
label_encoder_x = LabelEncoder()
x[:, 0] = label_encoder_x.fit_transform(x[:, 0])
one_hot_encoder = OneHotEncoder(categorical_features = [0])
x = one_hot_encoder.fit_transform(x).toarray()

# To prevent errors in processing, in python you should use one less column
# of categorical data than the number of options in your categorical feature.
# In this case, there are two options of categorical data, so we need one
# column of features. Known as "dummy variable"
x = x[:, 1:5] # removes the "dummy variable"

# Splitting the dataset into the Training set and Test set
x_train, x_test, y_train, y_test = train_test_split(
        x, y, test_size = 0.2, random_state = 0)

# Feature Scaling
sc_x = StandardScaler()
x_train = sc_x.fit_transform(x_train)
x_test = sc_x.transform(x_test)
sc_y = StandardScaler()
y_train = sc_y.fit_transform(y_train)

# Fitting Multiple Linear Regression to the Training set
regressor = LinearRegression()
regressor.fit(x_train, y_train)

# Prediction of the Test set results
y_pred = regressor.predict(x_test)

print(np.round(y_pred, 0))
print(y_test)


