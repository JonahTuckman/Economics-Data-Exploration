####################################################################
# Econ 277: ProblemSet2
# Author: Jonah Tuckman
# Date: February 11, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet2
# Purpose: Working through data in R, then python if time permits
####################################################################

install.packages("dplyr")
install.packages("ggplot2")
library("ggplot2")
library("dplyr")


setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet2')
## This line sets the working directory to the local path on my machine
images_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/ProblemSet2/Images'


#### Importing needed datasets
dataset <- read.csv('data/linked_1880_1920_males.csv') 


### removing unneeded variables
keep <- c("serial_1","year_1", "linktype", "namelast_1","namefrst_1","age_1","relate_1","sex_1","race_1","marst_1", "occscore_1","nativity_1")
dataset <- select(dataset, keep)

### Marking all fathers
dataset <- transform(dataset, father = ifelse (dataset$relate_1 == 'head/householder', 1, 0))
### Marking all children 
dataset <- transform(dataset, child = ifelse (dataset$relate_1 == 'child',1,0))

### Removing all non parents and non primaries
dataset <- subset(dataset, (dataset$linktype == 'primary link' | dataset$relate_1 == 'head/householder'))



### Question 1: 
# There are 20413 observations in my dataset after doing this. 
# I have variables to indicate children and fathers and they seem to be trading (0,1,0,1...) which is a good sign

### Question 2: How many siblings do you have in? (Hint these are people that share thesame serial_1)
# there are no siblings as we have solely kept primary link children and fathers

####### Assign occscore of father to child
dataset <- merge(dataset, dataset[dataset$father == 1, c("serial_1", "occscore_1")], by = "serial_1")


### Now we can drop fathers from the dataset
if (dataset$father == 1) {
  
}

dataset[dataset$father == '1',] <- NA
dataset <- na.omit(dataset)


