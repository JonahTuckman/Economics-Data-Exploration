# Data Source: 
# https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county3.csv



install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
library("ggpubr")
library("ggplot2")
library("dplyr")
install.packages("statar")
library("statar")

setwd('/Users/JonahTuckman/Desktop/Economics/Economics-Data-Exploration/GroupProject/')

# Data of Per Capita Income by County in 1959, 1969, 1979, 1989 pulled from census.gov
data <- read.csv('county3.csv')

# Data of California Data as we will be looking into California counties pre and post 1984 (Macintosh release)
CaliData <- subset(data, grepl('CA',State.and.County))
# Saved Dataset
write.csv(CaliData, "CaliData.csv")


# Bay Area/ High tech
### SanFran, San Mateo, Santaclara, Alameda, Contracosta, Solano, Napa, Sonoma, Marin
### LA County, Orange, San Diego

## Low Tech
### Others

CaliData <- read.csv("CaliData.csv")
toMatch <- c("San Francisco" , "San Mateo" , "Santa Clara" , "Alameda" , "Contra Costa" , "Solano" ,
             "Napa" , "Sonoma" , "Marin", "Los Angeles", "Orange County", "San Diego")
CaliHighIncome <- data.frame(CaliData)
CaliHighIncome <- subset(CaliHighIncome, grepl(paste(toMatch, collapse="|"), State.and.County))

CaliLowIncome <- data.frame(CaliData)
CaliLowIncome <- subset(CaliLowIncome, !grepl(paste(toMatch, collapse="|"), State.and.County))

## Replace the commas in the numbers to avoid importation as factor 
replaceCommas<-function(X){
  X<-as.numeric(gsub("\\,", "", X))
}

#replacement. Must do this indivudally to keep the commas in the state and county
# commenting after doing and realizing we are deleting that column in a moment so not needed...
CaliLowIncome$X1959 <- replaceCommas(CaliLowIncome$X1959)
CaliLowIncome$X1969 <- replaceCommas(CaliLowIncome$X1969)
CaliLowIncome$X1979 <- replaceCommas(CaliLowIncome$X1979)
CaliLowIncome$X1989 <- replaceCommas(CaliLowIncome$X1989)
CaliLowIncome$State.and.County <- NULL

write.csv(CaliLowIncome, "CaliLowIncome.csv")

## Removal of high income commas
CaliHighIncome$State.and.County <- NULL
CaliHighIncome$X1959 <- replaceCommas(CaliHighIncome$X1959)
CaliHighIncome$X1969 <- replaceCommas(CaliHighIncome$X1969)
CaliHighIncome$X1979 <- replaceCommas(CaliHighIncome$X1979)
CaliHighIncome$X1989 <- replaceCommas(CaliHighIncome$X1989)

write.csv(CaliHighIncome, "CaliHighTech2.csv")
### Removed Commas from numbers here to avoid importation as type factor
### Opened in text file, deleted commas, reopened in numbers and saved as CaliHighTech2.csv




############### Acount for Inflation here
############### We will use 1959 dollars in this study.

## According to the Bureau of Labor Statistics consumer price index, prices in 1969 are 26.12% higher than average prices throughout 1959.

CaliHighIncome$X1969 / .2612


CaliHighIncome$X <- NULL ## Adds X column of index from original list. Not needed
CaliHighIncome$State.and.County <- NULL ## No names (categorical variables suck to deal with)
summary(CaliHighIncome) # Checking out data

## Checking that everything is going well.
sapply(CaliHighIncome, class) ## Shows breakdown of columns by type


all(duplicated(CaliHighIncome$X1979)[-1L]) ## Test for constant vector
# Returns false which is good 


#### HIGH Income #### 

# Means of each year
averageHighIncome59 = mean(CaliHighIncome$X1959, na.rm = TRUE)
averageHighIncome69 = mean(CaliHighIncome$X1969, na.rm = TRUE)
averageHighIncome79 = mean(CaliHighIncome$X1979, na.rm = TRUE)
averageHighIncome89 = mean(CaliHighIncome$X1989, na.rm = TRUE)

#Standard deviation of each year
StandardDevHigh59 <- sd(CaliHighIncome$X1959, na.rm = TRUE)
StandardDevHigh69 <- sd(CaliHighIncome$X1969, na.rm = TRUE)
StandardDevHigh79 <- sd(CaliHighIncome$X1979, na.rm = TRUE)
StandardDevHigh89 <- sd(CaliHighIncome$X1989, na.rm = TRUE)


#### LOW Income #### 

# Means of each year
averageLowIncome59 = mean(CaliLowIncome$X1959, na.rm = TRUE)
averageLowIncome69 = mean(CaliLowIncome$X1969, na.rm = TRUE)
averageLowIncome79 = mean(CaliLowIncome$X1979, na.rm = TRUE)
averageLowIncome89 = mean(CaliLowIncome$X1989, na.rm = TRUE)

#Standard deviation of each year
StandardDevLow59 <- sd(CaliLowIncome$X1959, na.rm = TRUE)
StandardDevLow69 <- sd(CaliLowIncome$X1969, na.rm = TRUE)
StandardDevLow79 <- sd(CaliLowIncome$X1979, na.rm = TRUE)
StandardDevLow89 <- sd(CaliLowIncome$X1989, na.rm = TRUE)


# Visual Tabling
table59 <- table(averageHighIncome59, averageLowIncome59)
table59

table69 <- table(averageHighIncome69, averageLowIncome69)
table69

table79 <- table(averageHighIncome79, averageLowIncome79)
table79

table89 <- table(averageHighIncome89, averageLowIncome89)
table89


# Data to be used for plotting. Year vs Average in High vs Low Income
avgHigh <- data.frame("Year" = c(1959, 1969, 1979, 1989), 
                      "Average GDP Per Capita" = c(averageHighIncome59, averageHighIncome69,
                                                   averageHighIncome79, averageHighIncome89))
avgLow <- data.frame("Year" = c(1959, 1969, 1979, 1989), 
                     "Average GDP Per Capita" = c(averageLowIncome59, averageLowIncome69, 
                                                  averageLowIncome79, averageLowIncome89))

##### Plotting High Income
XHigh <- avgHigh$Year
YHigh <- log(avgHigh$Average.GDP.Per.Capita)

plotAvgHigh <- ggplot(data = avgHigh, mapping = aes(x = XHigh,y = YHigh)) + geom_point() + 
  geom_smooth(colour="darkred", size = 1)
plotAvgHigh + ggtitle("Average High Income") + xlab("Year") + ylab("Income Per Capita")

##### Plotting Low Income
XLow <- avgLow$Year
YLow <- log(avgLow$Average.GDP.Per.Capita)

plotAvgLow <- ggplot(data = avgLow, mapping = aes(x = XLow,y = YLow)) + geom_point() + geom_smooth(colour = "darkblue", size = 1)
plotAvgLow + ggtitle("Average Low Income") + xlab("Year") + ylab("Income Per Capita")

### Percent Growth 

#High
HighPerc69 <- ((averageHighIncome69 + averageHighIncome59) / averageHighIncome59)
HighPerc79 <- ((averageHighIncome79 + averageHighIncome69) / averageHighIncome69)
HighPerc89 <- ((averageHighIncome89 + averageHighIncome79) / averageHighIncome79)

#Low
LowPerc69 <- ((averageLowIncome69 + averageLowIncome59) / averageLowIncome59)
LowPerc79 <- ((averageLowIncome79 + averageLowIncome69) / averageLowIncome69)
LowPerc89 <- ((averageLowIncome89 + averageLowIncome79) / averageLowIncome79)

#Combine a dataset
Combined <- data.frame("Year" = c(1969, 1979, 1989),
                       "High" = c(HighPerc69, HighPerc79, HighPerc89),
                       "Low" = c(LowPerc69, LowPerc79, LowPerc89)
)

##### Combined Plot
combined <- ggplot() + 
  #High -> Red Plot
  geom_point(data=Combined, aes(x = Year, y = High)) + 
  geom_smooth(data=Combined, aes(x = Year, y = High), fill = "red", 
              colour="darkred", size = 1) +
  #Low -> Blue Plot
  geom_point(data=Combined, aes(x = Year, y = Low)) + 
  geom_smooth(data=Combined, aes(x=Year, y = Low), fill = "blue",
              colour = "darkblue", size = 1) +
  ggtitle("Combined Income Change") + xlab("Year") + ylab("% Change in Income per Capita") 
  # + geom_vline(xintercept=1980) + geom_vline(xintercept = 1970)
combined

dev.copy(png, "Combined.png")
dev.off()


### Dif n Dif regression 

Combined$HighxLow <- Combined$High*Combined$Low

# 2)Y=a+β1*(Income Group)+β2*(Mac Introduction)+β3*(Income Group*Mac Introduction)+e
summary(Combined)
CaliData$X1989 <- replaceCommas(CaliData$X1989)
CaliData$X1979 <- replaceCommas(CaliData$X1979)

CaliData <- transform(CaliData, HighBool = ifelse(match(CaliData$X1989, CaliHighIncome$X1989, nomatch = 0), 1, 0))
CaliData <- transform(CaliData, Grow = ifelse(CaliData$X1989 > CaliData$X1979, 1, 0))
CaliData <- transform(CaliData, Post89 = CaliData$X1989)
summary(CaliData)


YEARBASED$Income <- replaceCommas(YEARBASED$Income)

mean(YEARBASED$Income)
YEARBASED <- read.csv("YearBasedData.csv")
YEARBASED <- transform(YEARBASED, High = ifelse(YEARBASED$Income > 11098.78, 1,0))
YEARBASED <- transform(YEARBASED, After84 = ifelse(YEARBASED$Year > 1984,1,0) )

# nonLogHigh <- lm(log(CaliData$HighBool) + log(CaliData$X1989) + log(CaliData$X1989 * CaliData$HighBool) , data = CaliData)
  nonLogHigh <- lm(YEARBASED$High ~ YEARBASED$After84, data = CaliData)
  summary(nonLogHigh)
  
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        0.29885    0.03367   8.875  < 2e-16 ***
#    YEARBASED$After84  0.51149    0.06734   7.595 7.67e-13 ***

  YEARBASED<- transform(YEARBASED, AfterAndHigh = (YEARBASED$High * YEARBASED$After84 + YEARBASED$After84))
  Reformated <- lm(YEARBASED$High ~ YEARBASED$AfterAndHigh)
  summary(Reformated)

#                      Estimate Std. Error t value Pr(>|t|)    
 # (Intercept)             0.27974    0.03169   8.829 2.74e-16 ***
 # YEARBASED$AfterAndHigh  0.32476    0.03421   9.493  < 2e-16 ***

  
  DistanceModel <- lm(YEARBASED$Ddummy ~ YEARBASED$After84, data = YEARBASED)

  summary(DistanceModel)
  
 #                 Estimate Std. Error t value Pr(>|t|)    
 # (Intercept)        6.207e-01  3.694e-02    16.8   <2e-16 ***
  # YEARBASED$After84 -2.188e-16  7.389e-02     0.0        1    
  
  YEARBASED<- transform(YEARBASED, AfterAndFar = (YEARBASED$Ddummy * YEARBASED$After84 + YEARBASED$After84))
  
  ReformatedDist <- lm(YEARBASED$Ddummy ~ YEARBASED$AfterAndHigh)
  
 #                 Estimate Std. Error t value Pr(>|t|)    
 #  (Intercept)        6.207e-01  3.694e-02    16.8   <2e-16 ***
#    YEARBASED$After84 -2.188e-16  7.389e-02     0.0        1    
  
  