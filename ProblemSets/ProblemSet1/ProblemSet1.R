####################################################################
# Econ 277: ProblemSet1
# Author: Jonah Tuckman
# Date: February 11, 2019
# Location: ~/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet1
# Purpose: Working through data in R, then python if time permits
####################################################################

setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/ProblemSets/ProblemSet1')
## This line sets the working directory to the local path on my machine
images_dir <- '/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/ProblemSet1/Images'


#### Importing needed datasets
dataset <- read.csv('data/CountyRRData.csv') 
farmval <- read.csv('data/Dataset_1.1.csv')
cpiData <- read.csv('data/AnnualCPI_1800_2017.csv')

#### Installing needed packages
install.packages("tidyverse")
library("tidyverse")
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

### X = Year
X = combinedDataSet$YEAR
### Y = Ln(Adjusted Farm Values) 
Y = log(combinedDataSet$adjfarmval)

#3.1. Create a scatterplot with a local polynomial line for the ever RR group

### Ever Group
## group if there is a railroad
RRever = subset(combinedDataSet, `RR?` == 1)
# Year
XEver = RRever$YEAR
# Log of adjusted farm value in this subset
YEver = log(RRever$adjfarmval)
# plot of this grouping
plotEver <- ggplot(data = RRever, mapping = aes(x = XEver,y = YEver)) + geom_point() + geom_smooth()
plotEver + ggtitle("Ever Plot") + xlab("Year") + ylab("Log of Adjusted Farm Value")
## Copying image into directory to be added into images directory
dev.copy(png, 'PlotEver.png')
dev.off()

#3.2. Create a scatterplot with a local polynomial line for the never RR group.

## Dataset of counties which did not recieve a railroad 
RRnever = subset(combinedDataSet, `RR?` == 0)
Xnever = RRnever$YEAR
### Log of adjusted farm value
Ynever = log(RRnever$adjfarmval)
# plot of this grouping
plotNever <- ggplot(data = RRnever, mapping = aes(x = Xnever, y = Ynever)) + geom_point() + geom_smooth()
plotNever + ggtitle("Never Plot") + xlab("Year") + ylab("Log of Adjusted Farm Value")
## Copying image into directory to be added into images directory
dev.copy(png, "PlotNever.png")
dev.off()

"3.3. Create a scatterplot with the entire dataset in one graph. This should have the points
plotted in different colors and the lines in different colors. This is so we can start to see
how different the two areas are."

## Run together as a grouping. 
## Total dataset, made into factor
combinedDataSet$`RR?` <- as.factor(combinedDataSet$`RR?`)

## Log of adjusted farm values
YCombined <- log(combinedDataSet$adjfarmval)
plotCombined <- ggplot(data = combinedDataSet, aes(x = YEAR, y = YCombined,
                                                   color = combinedDataSet$`RR?`,
                                                   shape = combinedDataSet$`RR?`)) + geom_point() + geom_smooth()
plotCombined + ggtitle("Never and Ever Combined Plot") + xlab("Year") + ylab("Log of Adjusted Farm Value")
dev.copy(png, "NeverAndEverCombined.png")
dev.off()

#################################################
# Part 4: Basic Difference – in – Difference (DID) Models
#################################################

#4.1. Generate a new variable, after, which is equal to 1 if the year>1885 and 0 if year<=1885.

### Adding variable column based on if year is before or after 1985
combinedDataSet[, '1985'] <- ifelse(combinedDataSet['YEAR'] > 1885, 1, 0)


#4.2. Generate the interaction term for the DID

#### Railroads is interaction term (X), will run with different outcomes
RR1985 <- combinedDataSet$`1985`
## Adding RRNonFactor, same as RR? but non factor version 
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

### Dif in Dif  linear model
DIDSecond <- lm(YFarmLog ~ `RR?` + `1985` + XInt, data = combinedDataSet)
summary(DIDSecond)

#Question 8: Interpret the four coefficients of interest in this regression.

### The four coefficients of interest in this regression are Intercept, 'RR?', '1985' the interaction coefficient (X Interaction).
### When analyzing a regression model, the coefficients imply the weight associated with each and essentially indicate the importance
### of each variable. The coeficient of RR? implies that before the rail roads were introduced in 1985, there was a farmvalue of .47 units.
### Then after 1985 when trains were introduced, there is a weight / coefficient of .61 which is an increase from that prior to the innovation. 



#Question 9: Discuss the merits of this model. Do you think that this is a reasonable DID
#model based on the fundamental assumptions?

### When answering this question we have to analyze if there is a parallel trend between the two datasets prior to the introduction 
### of the inovative action. In this case the time period is short enough and the trend seems to be correlated enoguh (with similar coefficients)
### to imply a correlation between the two. 
### Considering all of our coefficient variables are lower than 1%, there is significantly more justification behind our presumption of a correlation
### between the variables prior to rail road introcution followed by a change post rail road introduction.

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

AdjFarmVsRRTotDist <- ggplot(data = combinedDataSet, aes(x = XAdjFarmVal, y = YAdjFarmVal)) + geom_point() + geom_smooth()
AdjFarmVsRRTotDist + ggtitle("Adjusted Farm Values vs Rail Road Initial Total Distance") + xlab("RR Initial Total Distance") + ylab("Adjusted Farm Values")
dev.copy(png, "Adjusted Farm Values vs Rail Road Initial Total Distance")
dev.off()

# Y is same
XNewAdjFarmVal = combinedDataSet$NewTotalDist

AdjFarmvsNewDist <- ggplot(data = combinedDataSet, aes(x = XNewAdjFarmVal, y = YAdjFarmVal)) + geom_point() + geom_smooth()
AdjFarmvsNewDist + ggtitle("Adjusted Farm Values vs Log of Rail Road Initial Total Distance + 1") + xlab("Log RR Initial Total Distance + 1") + ylab("Adjusted Farm Values")
dev.copy(png, "Adjusted Farm Values vs Log of Rail Road Initial Total Distance + 1")
dev.off()

# Question 10: Describe the patterns in the two figures above.

### In the initial plot, before adjusting our X value, there is a concentration of points primarily ranging from X = 0 -> X = 500.
### When the change of our X value is made, there is still a concentration of points, but now they concentrate in the center
### of the plot and around a much smaller range. In this they are in the center of the plot and primarily ranging from 
### X = 2 -> X = 6.

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

### In the first mdoel, the only coefficients are the Intercept and the XAdjusted Farm Value variable which we created.
### This initially is the Rail Road initial total distance. In this case the coefficient of the values is very small (2.629e-3 
### showing minial significance mathematically).
### In the second model, the coefficient is our adjusted value of the new total distance which is the log of the initial total distance + 1.
### This coefficient is small (.218) but certainly larger than the other which shows a higher significance level. 

# Question 12: Discuss the type of endogeneity concerns you would have regarding these
# models.
### Endogeneity concerns that I have are that railroad development are more likely in places of high growth, thus higher innovation and gains 
### may have been coming to these counties already.
### Mathematically a Std. error being incredibly low is not always a great thing because it can show overfitting in a model. In predictive models
### this is dangerous and in regression models this can lead to a guess line that finds trends that may not truly be present. 