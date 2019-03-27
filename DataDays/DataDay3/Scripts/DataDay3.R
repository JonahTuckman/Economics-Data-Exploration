#####################################################
# Econ 277: Data Day 3
# Author: Jonah Tuckman
# Date Created: 03/24/2019
# Date Last Modified: 03/27/2019
# Data File Locations: ~/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3
# Purpose: -Practice creating a script file
# -Familiarize self with basic R commands
# -Import and work with agricultural census data
# Notes: Using your script file to save time on dataday3.
#####################################################

#####################################################
# Set Directories
#####################################################
#Set Working Directory
setwd("~/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3")
#Raw Data Directory
rawdata_dir <- "~/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3/RawData"
#Images Directory
images_dir <- "~/Desktop/Economics/Economics-Data-Exploration/DataDays/DataDay3/Images"

#####################################################  
# Load Packages
#####################################################
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library("tidyverse")
library("ggplot2")
library("dplyr")
#####################################################  
# Import Data
#####################################################
rawlinkeddata <- read.csv('RawData/linked_1880_1920_males.csv')

#Keep people with data in both periods
finallinkeddata <- subset(rawlinkeddata, year_1==1880 & year_2==1920)


#####################################################  
# Histogram Code
#####################################################

hist1 <- ggplot(data=finallinkeddata, mapping = aes(x=occscore_1)) +
  geom_histogram()
hist1
dev.copy(png, "hist1.png")
dev.off()

hist2 <- ggplot(data=finallinkeddata, mapping = aes(x=occscore_2)) +
  geom_histogram()  
hist2
dev.copy(png, "hist2.png")
dev.off()

hist3 <- ggplot(data=finallinkeddata, mapping = aes(x=occdif)) +
  geom_histogram()
hist3
dev.copy(png, "hist3.png")
dev.off()

#####################################################  
# Add Variables
#####################################################  
#Create Occupational Score Change
finallinkeddata <- transform(finallinkeddata, occ_change = occscore_2 - occscore_1)

hist4 <- ggplot(data=finallinkeddata, mapping = aes(x=occ_change)) +
  geom_histogram()
hist4
dev.copy(png, "hist4.png")
dev.off()

#Create Nativity Variable
finallinkeddata <- transform(finallinkeddata, nat = ifelse(nativity_1=="native born, and both parents native born", 1, 
                                                           ifelse(nativity_1=="foreign born", 3,
                                                                  ifelse((nativity_1!="native born, and both parents native born" & nativity_1!="foreign born"), 2,4))))
#Create Nativity Dummies
finallinkeddata <- transform(finallinkeddata, natdum1 = ifelse(nat == 1, 1, 0))
finallinkeddata <- transform(finallinkeddata, natdum2 = ifelse(nat == 2, 1, 0))
finallinkeddata <- transform(finallinkeddata, natdum3 = ifelse(nat == 3, 1, 0))

#Create Race Dummy
finallinkeddata <- transform(finallinkeddata, racedum = ifelse(race_1 == "white", 0, 1))

#Create Marriage Dummy
finallinkeddata <- transform(finallinkeddata, marrdum = ifelse(marst_2 == "married, spouse absent", 1, 
                                                               ifelse(marst_2 =="married, spouse present", 1, 0)))

#Create Age-Squared
finallinkeddata <- transform(finallinkeddata, age_sq = age_clean^2)

#####################################################  
# Boxplot Code
#####################################################

boxy = ggplot(data=finallinkeddata, mapping = aes(x=factor(nat), y=occ_change)) + 
  geom_boxplot()
boxy 
dev.copy(png, "boxy.png")
dev.off()

boxy2 = ggplot(data=finallinkeddata, mapping = aes(x=factor(racedum), y=occ_change)) + 
  geom_boxplot()  
boxy2
dev.copy(png, "boxy2.png")
dev.off()

#####################################################  
# Linear Models
#####################################################  
reg1 <- lm(occ_change ~ natdum1 + natdum2 + natdum3 + age_clean + age_sq, data=finallinkeddata)

reg2 <- lm(occ_change ~ natdum2 + natdum3 + racedum + age_clean + age_sq, data=finallinkeddata)

reg3 <- lm(occ_change ~ natdum2 + natdum3 + age_clean + racedum + marrdum + age_sq, data=finallinkeddata)

#####################################################  
# Restricting Age
#####################################################     
hist5 <- ggplot(data=subset(finallinkeddata, age_clean>50), mapping = aes(x=occ_change)) +
  geom_histogram()  
hist5
dev.copy(png, "hist5.png")
dev.off()

reg4 <- lm(occ_change ~ natdum2 + natdum3 + age_clean + racedum + marrdum + age_sq, data=subset(finallinkeddata, age_clean>50))
