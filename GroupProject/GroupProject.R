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
 
### Removed Commas from numbers here to avoid importation as type factor
### Opened in text file, deleted commas, reopened in numbers and saved as CaliHighTech2.csv

CaliHighIncome <- read.csv("CaliHighTech2.csv")
CaliHighIncome$X <- NULL ## Adds X column which is empty
CaliHighIncome$State.and.County <- NULL ## No names (categorical variables suck to deal with)
summary(CaliHighIncome)

sapply(CaliHighIncome, class) ## Shows breakdown of columns by type


all(duplicated(CaliHighIncome$X1979)[-1L]) ## Test for constant vector
# Returns false which is good 


# Means of each year
averageHighTech59 = mean(CaliHighIncome$X1959, na.rm = TRUE)
averageHighTech69 = mean(CaliHighIncome$X1969, na.rm = TRUE)
averageHighTech79 = mean(CaliHighIncome$X1979, na.rm = TRUE)
averageHighTech89 = mean(CaliHighIncome$X1989, na.rm = TRUE)

#Standard deviation of each year
StandardDev59 <- sd(CaliHighIncome$X1959, na.rm = TRUE)
StandardDev69 <- sd(CaliHighIncome$X1969, na.rm = TRUE)
StandardDev79 <- sd(CaliHighIncome$X1979, na.rm = TRUE)
StandardDev89 <- sd(CaliHighIncome$X1989, na.rm = TRUE)
