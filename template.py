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

# Import dataset
dataset = pd.read_csv('mall_customers.csv')
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
# column of features.
x = x[:, 1:5] # drops the first column of categorical features.






