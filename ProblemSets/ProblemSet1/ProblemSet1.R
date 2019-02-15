####################################################################
# Econ 277: ProblemSet1
# Author: Jonah Tuckman
# Date: February 11, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet1
# Purpose: Working through data in R, then python if time permits
####################################################################

setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet1')
## This line sets the working directory to the local path on my machine

#### Importing needed datasets
dataset <- read.csv('data/CountyRRData.csv') 
farmval <- read.csv('data/Dataset_1.1.csv')
cpiData <- read.csv('data/AnnualCPI_1800_2017.csv')

#### Installing needed packages
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
install.packages("ggplot2")
library("ggplot2")
library("dplyr")

#################################################
## PART 1:Dataset Construction ##
#################################################

# Question 1: What is a unit of observation in the data?
### A unit of observation in the data is kilometers in terms of rail road distance. 

# 1.1 Construct a county-level panel dataset with farm values and railroad kms. (Hint: Make
# sure you drop/delete any unnecessary data.)

## Reading dataset from dataday1
combinedDataSet <- read.csv('data/DataDay1CountyData.csv')
### Adding adjfarmval variable into dataset
combinedDataSet <- transform(combinedDataSet, adjfarmval = (FAVAL/(Annual.Average/100)))
### Updating column names
colnames(dataset) <- c("FIPS", "RRinitialtotaldist")
### adding countyrailroad data to dataset
combinedDataSet <- merge(combinedDataSet, dataset, by = "FIPS")

# 1. 2. Now create an indicator (dummy) variable for whether or not the county ever receives a
# railroad.

### Adding variable which shows if a county had a railroad
combinedDataSet[, 'RR?'] <- ifelse(combinedDataSet['RRinitialtotaldist'] == 0, 0, 1)

# 3. We want to focus on the era of railroad expansion so drop all observations after 1930

### removing all years after 1930
combinedDataSet <- subset(combinedDataSet, (YEAR <= 1930))

# Question 2: How many observations are in your dataset after this adjustment?
nrow(combinedDataSet)
### Number of rows / number of observations: 30,770

# Question 3: How many missing land values do you have?
summary(combinedDataSet, na.rm = TRUE)
### 5521 NA's

#################################################
## Part 2: Summary Statistics ##
#################################################

# 1. Generate a table of summary statistics for adjustment farm values, year, your new
# binary railroad measure, and the railroad km measure to answer the following
# questions meanFarmVal = mean(combinedDataSet$adjfarmval, na.rm = TRUE)

noRR = subset(combinedDataSet,`RR?` == 0)
numberNoRR = nrow(noRR)
total = nrow(combinedDataSet)
percentNoRailRoad = (numberNoRR / total) * 100

avgRRKM = mean(combinedDataSet$RRinitialtotaldist, na.rm = TRUE)

standardDevRRKM = sd(combinedDataSet$RRinitialtotaldist , na.rm=TRUE)

table1 <- table(meanFarmVal, percentNoRailRoad, avgRRKM, standardDevRRKM)
table1

#Question 4: What is the mean inflation adjusted farm value?
print(meanFarmVal)
#104.7715

#Question 5: What percent of counties did not receive any railroad?
print(percentNoRailRoad)
# 4.68%

#Question 6: What is the average kms of railroad?
print(avgRRKM)
# 126.35 kms

#Question 7: What is the standard deviation of the railroad kms?
print(standardDevRRKM)
# 101.62


#################################################
#Part 3: Scatterplots with Binary RR Treatment 
#################################################

### 
X = combinedDataSet$YEAR
Y = log(combinedDataSet$adjfarmval)

#3.1. Create a scatterplot with a local polynomial line for the ever RR group
RRever = subset(combinedDataSet, `RR?` == 1)
XEver = RRever$YEAR
YEver = log(RRever$adjfarmval)
plotEver <- ggplot(data = RRever, mapping = aes(x = XEver,y = YEver)) + geom_point() + geom_smooth()
plotEver

#3.2. Create a scatterplot with a local polynomial line for the never RR group.
RRnever = subset(combinedDataSet, `RR?` == 0)
Xnever = RRnever$YEAR
Ynever = log(RRnever$adjfarmval)
plotNever <- ggplot(data = RRnever, mapping = aes(x = Xnever, y = Ynever)) + geom_point() + geom_smooth()
plotNever

"3.3. Create a scatterplot with the entire dataset in one graph. This should have the points
plotted in different colors and the lines in different colors. This is so we can start to see
how different the two areas are."

## Run together as a grouping. 
combinedDataSet$`RR?` <- as.factor(combinedDataSet$`RR?`)

YCombined <- log(combinedDataSet$adjfarmval)
plotCombined <- ggplot(data = combinedDataSet, aes(x = YEAR, y = YCombined,
                                                   color = combinedDataSet$`RR?`,
                                                   shape = combinedDataSet$`RR?`)) + geom_point() + geom_smooth()
  
plotCombined

#################################################
# Part 4: Basic Difference – in – Difference (DID) Models
#################################################

#4.1. Generate a new variable, after, which is equal to 1 if the year>1885 and 0 if year<=1885.
combinedDataSet[, '1985'] <- ifelse(combinedDataSet['YEAR'] > 1885, 1, 0)


#4.2. Generate the interaction term for the DID
#### Railroads is interaction term (X), will run with different outcomes
RR1985 <- combinedDataSet$`1985`
combinedDataSet$RRNonFactor <- ifelse(combinedDataSet['RRinitialtotaldist'] > 0,1,0) # needed for multiplication 
                                                                                    # Because RR? is a factor now
RRBinary <- combinedDataSet$RRNonFactor
combinedDataSet$XInteraction <- (RRBinary * RR1985)


#4.3. Run a standard DID model using adjusted farm values as the outcome
YFarm = combinedDataSet$adjfarmval
XInt = combinedDataSet$XInteraction

DIDFirst <- lm(YFarm ~ `RR?` + `1985` + XInt, data = combinedDataSet)
summary((DIDFirst))

#4.4. Run a standard DID model using the natural log of adjusted farm values as the outcome
combinedDataSet$YFarmLog = log(combinedDataSet$adjfarmval)


DIDSecond <- lm(YFarmLog ~ `RR?` + `1985` + XInt, data = combinedDataSet)
summary(DIDSecond)

#Question 8: Interpret the four coefficients of interest in this regression.
#Question 9: Discuss the merits of this model. Do you think that this is a reasonable DID
#model based on the fundamental assumptions?


#################################################
# Part 5: Generate New KM Based Graphs
#################################################

# 5.1 Generate a new measure of railroad km. This new measure should equal the
# Ln(RRinitialtotaldist + 1)

combinedDataSet$NewTotalDist = log(combinedDataSet$RRinitialtotaldist + 1)


# 5.2. Create two new scatterplots with polynomial lines. For the first plot let Y = Ln(AdjustedFarm Values) & X = RRinitialtotaldist.
# For the second plot let Y = Ln(Adjusted FarmValues) , X = your new log measure.

YAdjFarmVal = log(combinedDataSet$adjfarmval)
XAdjFarmVal = combinedDataSet$RRinitialtotaldist

plot1 <- ggplot(data = combinedDataSet, aes(x = XAdjFarmVal, y = YAdjFarmVal)) + geom_point() + geom_smooth()
plot1

# Y is same
XNewAdjFarmVal = combinedDataSet$NewTotalDist

plot2 <- ggplot(data = combinedDataSet, aes(x = XNewAdjFarmVal, y = YAdjFarmVal)) + geom_point() + geom_smooth()
plot2


# Question 10: Describe the patterns in the two figures above.

#################################################
#Part 6: Run Simple Bivariate Regressions
#################################################

# Building regressions similar to part 5 plots

# 6.1. For the first regression, let Y = Ln(Adjusted Farm Values) & X = RRinitialtotaldist.
XAdjFarmVal = combinedDataSet$RRinitialtotaldist

nonLogReg <- lm(YAdjFarmVal ~ XAdjFarmVal, data = combinedDataSet )
summary(nonLogReg)

# 6.2. For the second regression, let Y = Ln(Adjusted Farm Values) , X = your new log measure

XNewAdjFarmVal = combinedDataSet$NewTotalDist
LogReg <- lm(YAdjFarmVal ~ XNewAdjFarmVal, data = combinedDataSet)
summary(LogReg)


# Question 11: Interpret the coefficients of interest from both models.
# Question 12: Discuss the type of endogeneity concerns you would have regarding these
# models.