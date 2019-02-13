####################################################################
# Econ 277: ProblemSet1
# Author: Jonah Tuckman
# Date: February 11, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet1
# Purpose: Working through data in R, then python if time permits
####################################################################

setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet1')
## This line sets the working directory to the local path on my machine

dataset <- read.csv('data/CountyRRData.csv')
farmval <- read.csv('data/Dataset_1.1.csv')
cpiData <- read.csv('data/AnnualCPI_1800_2017.csv')

combinedDataSet <- read.csv('data/DataDay1CountyData.csv')
combinedDataSet <-transform(combinedDataSet, adjfarmval = (FAVAL/Annual.Average/100))
combinedDataSet <- left_join(combinedDataSet, dataset, by = c("FIPS" = "fips"))

## PART 1:Dataset Construction ##

# Question 1: What is a unit of observation in the data?
### A unit of observation in the data is kilometers in terms of rail road distance. 


combinedDataSet[, 'RR?'] <- ifelse(combinedDataSet['RRinitialtotaldist'] == 0, 0, 1)
combinedDataSet <- subset(combinedDataSet, (YEAR <= 1930))
# Question 2: How many observations are in your dataset after this adjustment?
### 30,770
# Question 3: How many missing land values do you have?
summary(combinedDataSet, na.rm = TRUE)
### 5521 NA's

## Part 2: Summary Statistics ##

meanFarmVal = mean(combinedDataSet$adjfarmval, na.rm = TRUE)

noRR = subset(combinedDataSet,`RR?` == 0)
numberNoRR = nrow(noRR)
total = nrow(combinedDataSet)
percentNoRailRoad = (numberNoRR / total) * 100

avgRRKM = mean(combinedDataSet$RRinitialtotaldist, na.rm = TRUE)

standardDevRRKM = sd(combinedDataSet$RRinitialtotaldist , na.rm=TRUE)

#Question 4: What is the mean inflation adjusted farm value?
print(meanFarmVal)
#.01047715

#Question 5: What percent of counties did not receive any railroad?
print(percentNoRailRoad)
# 4.68%

#Question 6: What is the average kms of railroad?
print(avgRRKM)
# 126.35 kms

#Question 7: What is the standard deviation of the railroad kms?
print(standardDevRRKM)
# 101.62


## Part 3: Scatterplots with Binary RR Treatment ## 
X = combinedDataSet$YEAR
