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

## PART1 ##

# Question 1: What is a unit of observation in the data?
### A unit of observation in the data is kilometers in terms of rail road distance. 


combinedDataSet[, 'RR?'] <- ifelse(combinedDataSet['RRinitialtotaldist'] == 0, 0, 1)

