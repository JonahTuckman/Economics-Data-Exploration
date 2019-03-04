# Data Source: 
# https://www2.census.gov/programs-surveys/decennial/tables/time-series/historical-income-counties/county3.csv



install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
install.packages("ggplot2")
library("ggplot2")
library("dplyr")

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


toMatch <- c("San Francisco" , "San Mateo" , "Santa Clara" , "Alameda" , "Contra Costa" , "Solano" ,
             "Napa" , "Sonoma" , "Marin", "Los Angeles", "Orange County", "San Diego")
CaliHighTech <- data.frame(CaliData)
CaliHighTech <- subset(CaliHighTech, grepl(paste(toMatch, collapse="|"), State.and.County))
write.csv(CaliHighTech, "CaliHighTech.csv")

CaliHighIncome <- read.csv("CaliHighTech.csv")
CaliHighIncome$X <- NULL
CaliHighIncome$State.and.County <- NULL ## No names (categorical variables suck to deal with)
summary(CaliHighIncome)

## Convertion bellow ################################################

testdf <- as.data.frame(lapply(CaliHighIncome, factor))
str(testdf)

testdf[] <- lapply(testdf, function(x)
  as.numeric(levels(x))[x])


summary(testdf)
sapply(testdf, class)

CaliHighIncome <- transform(CaliHighIncome, class=as.numeric(as.character(CaliHighIncome)))



sapply(CaliHighIncome, class)
sapply(CaliData, class)


CaliHighIncome$X1959 <- as.numeric(as.character(CaliHighIncome$X1959))

CaliHighIncome$X1969 <- as.numeric(CaliHighIncome$X1969)
CaliHighIncome$X1979 <- as.numeric(CaliHighIncome$X1979)
CaliHighIncome$X1989 <- as.numeric(CaliHighIncome$X1989)

summary(CaliHighIncome)

##########################################################################
all(duplicated(CaliHighIncome$X1979)[-1L]) ## Test for constant vector

averageHighTech79 = lapply(CaliHighIncome, mean, na.rm = TRUE)
StandardDev79 <- sd(CaliHighIncome$X1959, na.rm = TRUE)
