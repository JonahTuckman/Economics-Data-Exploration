####################################################################
# Econ 277: ProblemSet2
# Author: Jonah Tuckman
# Date: February 11, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet2
# Purpose: Working through data in R, then python if time permits
####################################################################

### Updating r version for xtile
library("devtools")
install_github('andreacirilloac/updateR')
library(updateR)


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
dataset[dataset$father == '1',] <- NA
dataset <- na.omit(dataset)

### Question 3: How many observations are in your dataset after these adjustments?
# There are 9581 observations in the study now 

install.packages("statar")
library("statar")

### O score percentiles
dataset['Father Percentile'] <- xtile(dataset$occscore_1.x, n = 100)
dataset['Son Percentile'] <- xtile(dataset$occscore_1.y, n = 100)

dataset['Level Change'] <- dataset$occscore_1.y - dataset$occscore_1.x
dataset['Rank Change'] <- dataset$`Son Percentile` - dataset$`Father Percentile`
dataset <- transform(dataset, LevelUp = ifelse (dataset$occscore_1.y > dataset$occscore_1.x,1,0))
dataset <- transform(dataset, RankUp = ifelse (dataset$Son.Percentile > dataset$Father.Percentile,1,0))

#  Question 4: What is the mean occscore for sons?
print(mean(dataset$occscore_1.x))
### 4.22 

#  Question 5: What is the mean occscore for fathers?
print(mean(dataset$occscore_1.y))
### 18.2

#  Question 6: What is the mean rank for sons?
print(mean(dataset$Son.Percentile))
### 35.55 %

#  Question 7: What is the mean rank for fathers?
print(mean(dataset$Father.Percentile))
### 22.83%

#  Question 8: What is the mean level change?
print(mean(dataset$Level.Change))
### 14.06 %

#  Question 9: What is the mean rank change?
print(mean(dataset$Rank.Change))
### 12.7% 

#  Question 10: What fraction of the sample had their rank improve?
count = 0
for (val in dataset$Rank.Change){
  if (val > 0) {
    count = count + 1
  } else {
    count = count
  }
}

print(100 * (count/9583))
### 70.67%


### Confidence interval plot
plot(dataset$Father.Percentile, ylim = c(0,100), type = "l")
library(scales) # For the percent_format() function

scale_y_continuous(labels = percent_format(), limits=c(0,1))

LVLPLOT <- ggplot(data = dataset, mapping = aes(x = dataset$Father.Percentile, y = dataset$Level.Change))
LVLPLOT + geom_point() + geom_smooth() + scale_y_continuous(label = percent)

RNKPLOT <- ggplot(data = dataset, mapping = aes(x = dataset$Father.Percentile, y = dataset$Rank.Change))
RNKPLOT + geom_point() + geom_smooth() + scale_y_continuous(label = percent)

LVLUPPLOT <- ggplot(data = dataset, mapping = aes(x = dataset$Father.Percentile, y = dataset$LevelUp))
LVLUPPLOT + geom_point() + geom_smooth() + scale_y_continuous(label = percent)

RNKUPPLOT <- ggplot(data = dataset, mapping = aes(x = dataset$Father.Percentile, y = dataset$RankUp))
RNKUPPLOT + geom_point() + geom_smooth() + scale_y_continuous(label = percent)

dataset <- transform(dataset, white = ifelse (dataset$race_1 =='white',1,0))

RACERNKUP <- ggplot(data = dataset, mapping = aes(x = dataset$Rank.Change, y = dataset$white, method = "loess"))
RACERNKUP + geom_point() + geom_smooth() + scale_y_continuous(label = percent)





