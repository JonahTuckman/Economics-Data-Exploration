####################################################################
# Econ 277: Data Day 1
# Author: Jonah Tuckman
# Date: February 4, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/DataDay1
# Purpose: Going through process of building python code
####################################################################


setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay1')
## This line sets the working directory to the local path on my machine
rawdata_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay1/RawData'

images_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay1/Images'


# Questions for you to check your understand: 
# What is the difference between a working directory and these other directories? 
### A working dirrectory is where your script is locally run thus paths will be
### according to this file

# What is the ~ doing? 
### ~ makes is a global command and links any file to find this

# What is the <- doing?
### Pointer assignment in R.

library(tidyverse)
install.packages('tidyverse')
install.packages('dplyr')
library(dplyr)

dataset <- read.csv('RawData/DataSet1_AgricFarmVal.csv')

nrow(dataset)
nrow(dataset$FAVAL)
nrow(dataset$FIPS)
summary(dataset$FAVAL)
countydata <- subset(dataset, LEVEL = 1)
countydata <- subset(dataset, LEVEL == 1)

x <- countydata$YEAR
y <- countydata$FAVAL
plot(x = countydata$YEAR, y = countydata$FAVAL)
abline(lm(y ~ x, data = countydata), col = 'blue')

cpiData <- read.csv('RawData/AnnualCPI_1800_2017.csv')
countydata2 <- left_join(x = countydata,y = cpiData, by = c('YEAR' = 'Year'))
write.csv(countydata2, file = 'MostRecentFile.csv')