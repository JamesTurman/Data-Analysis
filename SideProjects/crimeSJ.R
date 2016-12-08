# mapping crime in san francisco, ca from december 8, 2015 to now
# data is too large to handle on local machine... heat map will not populate with low data points
author <- "Jim Turman"
# load libraries
library(dplyr) # for data formatting 
library(ggmap) #for map
library(tidyr) #format data
library(ggplot2) #plotting

crime <- read.csv("crime_sf.csv")

# remove empty locations
crime$Location[crime$Location == ''] <- NA
crime <- na.omit(crime)

## Splitting location into latitude and longitude
crime <- extract(crime, Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')

# get map of SF
sf <- get_map(location='san francisco',zoom=14)
ggmap(sf)

#create data frame with coordinates of all crimes 
location.crime <- table(crime$Longitude, crime$Latitude)
location.crime <- data.frame(location.crime)
names(location.crime) <- c('long', 'lat', 'frequency')
location.crime$long <- as.numeric(as.character(location.crime$long))
location.crime$lat <- as.numeric(as.character(location.crime$lat))
location.crime <- subset(location.crime, location.crime$frequency > 0)

#overlay crimes on map of san francisco
ggmap(sf) + geom_tile(data = location.crime, aes(x = long, y = lat, alpha = frequency),
                           fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())









