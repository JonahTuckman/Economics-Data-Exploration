



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


