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

countydata <- read.csv('RawData/DataDay1CountyData.csv')
countydataSub <- subset(countydata, (YEAR >= 1870 & YEAR <= 1900))

AgricCensus <- read.csv('RawData/AgricCensus_1870_1900.csv')
# Overlaps with countydataSub with YEAR and FIPS
combined2 <- left_join(x = countydataSub,y = AgricCensus, by = c('YEAR' = 'YEAR'), suffix = c('.x', '.y'))
landSuit <- read.csv('RawData/suit_wheat.csv')
farmval_production_suit = left_join(x = combined2, y = landSuit, by = c("FIPS.y" = "fips"))