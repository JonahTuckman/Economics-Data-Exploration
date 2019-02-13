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
agrData <- read.csv('data/AgricCensus_1870_1900.csv')

combinedDataSet = left_join(x = agrData, y = dataset, by = c("FIPS" = "fips"))

## PART 1:Dataset Construction ##

# Question 1: What is a unit of observation in the data?
### A unit of observation in the data is kilometers in terms of rail road distance. 


combinedDataSet[, 'RR?'] <- ifelse(combinedDataSet['RRinitialtotaldist'] == 0, 0, 1)

## Part 2: Summary Statistics ##

meanFarmVal = mean(combinedDataSet$FARMS, na.rm = TRUE)

noRR = subset(combinedDataSet,`RR?` == 0)
numberNoRR = nrow(noRR)
total = nrow(combinedDataSet)
percentNoRailRoad = (numberNoRR / total) * 100

avgRRKM = mean(combinedDataSet$RRinitialtotaldist, na.rm = TRUE)

standardDevRRKM = sd(combinedDataSet$RRinitialtotaldist , na.rm=TRUE)

#Question 4: What is the mean inflation adjusted farm value?
print(meanFarmVal)
#Question 5: What percent of counties did not receive any railroad?
print(percentNoRailRoad)
#Question 6: What is the average kms of railroad?
print(avgRRKM)
#Question 7: What is the standard deviation of the railroad kms?
print(standardDevRRKM)


## Part 3: Scatterplots with Binary RR Treatment ## 

