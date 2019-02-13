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
farmval <- read.csv('data/Dataset_1.1.csv')
cpiData <- read.csv('data/AnnualCPI_1800_2017.csv')

combinedDataSet <- read.csv('data/DataDay1CountyData.csv')
combinedDataSet <-transform(combinedDataSet, adjfarmval = (FAVAL/Annual.Average/100))
combinedDataSet <- left_join(combinedDataSet, dataset, by = c("FIPS" = "fips"))

## PART 1:Dataset Construction ##

# Question 1: What is a unit of observation in the data?
### A unit of observation in the data is kilometers in terms of rail road distance. 


combinedDataSet[, 'RR?'] <- ifelse(combinedDataSet['RRinitialtotaldist'] == 0, 0, 1)
combinedDataSet <- subset(combinedDataSet, (YEAR <= 1930))
# Question 2: How many observations are in your dataset after this adjustment?
### 30,770
# Question 3: How many missing land values do you have?
summary(combinedDataSet, na.rm = TRUE)
### 5521 NA's

## Part 2: Summary Statistics ##

meanFarmVal = mean(combinedDataSet$adjfarmval, na.rm = TRUE)

noRR = subset(combinedDataSet,`RR?` == 0)
numberNoRR = nrow(noRR)
total = nrow(combinedDataSet)
percentNoRailRoad = (numberNoRR / total) * 100

avgRRKM = mean(combinedDataSet$RRinitialtotaldist, na.rm = TRUE)

standardDevRRKM = sd(combinedDataSet$RRinitialtotaldist , na.rm=TRUE)

#Question 4: What is the mean inflation adjusted farm value?
print(meanFarmVal)
#.01047715

#Question 5: What percent of counties did not receive any railroad?
print(percentNoRailRoad)
# 4.68%

#Question 6: What is the average kms of railroad?
print(avgRRKM)
# 126.35 kms

#Question 7: What is the standard deviation of the railroad kms?
print(standardDevRRKM)
# 101.62


## Part 3: Scatterplots with Binary RR Treatment ## 
X = combinedDataSet$YEAR
Y = log(combinedDataSet$adjfarmval)

# 1. Create a scatterplot with a local polynomial line for the ever RR group
RRever = subset(combinedDataSet, `RR?` == 1)
XEver = RRever$YEAR
YEver = log(RRever$adjfarmval)
plot(XEver, YEver)
abline(lm(YEver ~ XEver, data = RRever), col = 'blue')

# 2. Create a scatterplot with a local polynomial line for the never RR group.
RRnever = subset(combinedDataSet, `RR?` == 0)
Xnever = RRnever$YEAR
Ynever = log(RRnever$adjfarmval)
plot(Xnever, Ynever)
abline(lm(Ynever ~ Xnever, data = RRnever), col = 'blue')

"3. Create a scatterplot with the entire dataset in one graph. This should have the points
plotted in different colors and the lines in different colors. This is so we can start to see
how different the two areas are."

## Run together as a grouping. 
plot(XEver, YEver,col = 'blue', xlab = 'X', ylab = 'Y', ylim = range(-10: 2))
par(new = TRUE, yaxs = "i") # Adds both plots to the single graph
plot(Xnever, Ynever, col = 'red', xlab = '', ylab = '', ylim = range(-10:2))
abline(lm(YEver ~ XEver, data = RRever), col = 'blue')
abline(lm(Ynever ~ Xnever, data = RRnever), col = 'red')

# Part 4: Basic Difference – in – Difference (DID) Models

#1. Generate a new variable, after, which is equal to 1 if the year>1885 and 0 if year<=1885.
combinedDataSet[, '1985'] <- ifelse(combinedDataSet['YEAR'] > 1885, 1, 0)
# 2. Generate the interaction term for the DID
#### Railroads is interaction term (X), will run with different outcomes
Xinter = combinedDataSet$`RR?`
# Xinter2 = combinedDataSet$RRinitialtotaldist

# 3. Run a standard DID model using adjusted farm values as the outcome
YFarm = combinedDataSet$adjfarmval
plot(Xinter, YFarm,
     type = "p",
     ylim = range(-1:3))
abline(lm(YFarm ~ Xinter, data = combinedDataSet), col = 'red')

# 4. Run a standard DID model using the natural log of adjusted farm values as the outcome
YFarmLog = log(combinedDataSet$adjfarmval)
plot(Xinter, YFarmLog,
     type = "p",
     ylim = range(-10:1))
abline(lm(YFarmLog ~ Xinter, data = combinedDataSet), col = 'red')



