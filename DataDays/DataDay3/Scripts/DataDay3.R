####################################################################
# Econ 277: Data Day 3
# Author: Jonah Tuckman
# Date: March 25, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3
####################################################################


setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3')
## This line sets the working directory to the local path on my machine
rawdata_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3/RawData'

images_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3/Images'

install.packages("tidyverse", dependencies =TRUE)
install.packages("DBI", dependencies = TRUE)
library(DBI)
install.packages("dplyr")
library(tidyverse)
library(dplyr)


dataset <- read.csv("RawData/linked_1880_1920_males.csv")
serial26500 <-  subset(dataset, (dataset$serial_2 == 6501))





