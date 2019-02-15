####################################################################
# Econ 277: Data Day 2
# Author: Jonah Tuckman
# Date: February 11, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/DataDay2
# Purpose: Working through data in R, then python if time permits
####################################################################



setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay2')
## This line sets the working directory to the local path on my machine
rawdata_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay2/RawData'

images_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDay2/Images'

library(tidyverse)
install.packages('tidyverse')
install.packages("dplyr")

countydata <- read.csv('RawData/DataDay1CountyData.csv')
countydataSub <- subset(countydata, (YEAR >= 1870 & YEAR <= 1900))

AgricCensus <- read.csv('RawData/AgricCensus_1870_1900.csv')
# Overlaps with countydataSub with YEAR and FIPS
combined2 <- left_join(x = countydataSub,y = AgricCensus, by = c('FIPS' = 'FIPS', 'YEAR' = 'YEAR'))
landSuit <- read.csv('RawData/suit_wheat.csv')
# Overlapping Fips are landSuit$fips and combined2$FIPS.y
farmval_production_suit = left_join(x = combined2, y = landSuit, by = c("FIPS" = "fips"))


#What are the new variables in your dataset?
#  To answer this one, I added a new codebook to the moodle page.
### The new variables in our combined dataset are FIPS.y, TotalPopulation (TOTPOP), acres of improved farm land (ACIMP),
### Cash value of farming machinery (EQUIPVAL), estimated value of all farm productions (FARMOUT),
### Total number of farms (FARMS), suitable wheat (suit_wheat)


#Are the values in real or nominal dollars?
###
# For the suitability measure, are larger numbers a sign of better land or worse land?
###

IAEquipVal = transform(farmval_production_suit$EQUIPVAL / (farmval_production_suit$Annual.Average / 100))
