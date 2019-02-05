####################################################################
# Econ 277: Data Day 1
# Author: Jonah Tuckman
# Date: February 4, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/DataDay1
# Purpose: Going through process of building python code
####################################################################


import pandas as pd ## Pandas is a great package for dealing with datasets
import numpy as np ## numpy great for array and vector work
import matplotlib.pyplot as plt ## will be used for plotting (clearly)
import os ## misscelaneous operating system interface
import matplotlib
#matplotlib.use('GTKAgg')
from sklearn import datasets, linear_model
os.chdir('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay1')
## This line sets the working directory to the local path on my machine


# Questions for you to check your understand: 
# What is the difference between a working directory and these other directories? 
### A working dirrectory is where your script is locally run thus paths will be
### according to this file

# What is the ~ doing? 
### ~ makes is a global command and links any file to find this

# What is the <- doing?
### Pointer assignment in R.




dataset = pd.read_csv('RawData/DataSet1_AgricFarmVal.csv') # reading datafile and 
                                                        # attatching to variable dataset
                                             
dataset.head() # Shows the first five rows of the dataset
print(dataset)



#   FIPS         NAME  YEAR  FAVAL  LEVEL
#0  9000  CONNECTICUT  1850   31.0      2
#1  9000  CONNECTICUT  1860   36.0      2
#2  9000  CONNECTICUT  1870   42.0      2
#3  9000  CONNECTICUT  1880   49.0      2
#4  9000  CONNECTICUT  1890   42.0      2

# what type of data is this? 
### This data is 4 columns of numerical data and one columns of categorical data

# What is a unit of observation? 
### Name is the changing categorical column

# Another way to ask that question is to ask, what is the data unique by?
### Name?

total_rows = dataset.count
print(total_rows) # shows [49936 rows x 5 columns]


# total FAVAL rows
total_faval = dataset['FAVAL'].count
print(total_faval)

# plt.plot(dataset['NAME'], label = 'Counties')
np.nan_to_num(dataset)
np.nan_to_num(dataset['FAVAL'])
dataset.isnull().sum().sum()

nan_rows = dataset[dataset['FAVAL'].isnull()]
print(nan_rows)

# Create dictionary of state names, do not add if in this array
#for i in range(0, len(dataset)):
 #   new_data['NAME'][i] = dataset['NAME'][i]

county_data = dataset.query('LEVEL == 1')
# Our new data is different because there are no level 2's (states)



x = county_data['YEAR'].values # done with .values to keep it in type int rather than a series
perc = len(x) * .8 # 80% of length is standard in training regression models
print(perc)
# 39335
x = x.reshape(len(x), 1) # need to be a 1d array for regression prediction

y = county_data['FAVAL'].values # done with .values to keep it in type float rather than a series
y = y.reshape(len(y), 1)

plt.scatter(county_data['YEAR'], county_data['FAVAL'], c = 'RED')

x_train = x[:39335]
x_train = np.nan_to_num(x_train)
np.isnan(x_train).sum()

x_test = x[39335:]
x_test = np.nan_to_num(x_test)
np.isnan(x_test).sum()

y_train = y[:39335]
y_train = np.nan_to_num(y_train)
np.isnan(y_train).sum()

y_test = y[39335:]
y_test = np.nan_to_num(y_test)
np.isnan(y_test).sum()

regression = linear_model.LinearRegression()
regression.fit(x_train, y_train)
prediction = regression.predict(x_test)

plt.scatter(x_test, y_test, color = 'BLACK')
plt.title('Test Data Counties')
plt.xlabel('Year')
plt.ylabel('FAVAL')
plt.xticks(())
plt.yticks(())
plt.plot(x_test, prediction, color = 'RED', linewidth = 3)
plt.show()

CPI_Data = pd.read_csv('RawData/AnnualCPI_1800_2017.csv')
