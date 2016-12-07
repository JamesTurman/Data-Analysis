library(twitteR)
library(data.table)

consumer_key <- "your_consumer_key"
consumer_secret <- "your_consumer_secret"
access_token <- "your_acces_token"
access_secret <- "your_access_secret"
#This will enable the use of a local file to cache OAuth access credentials between R sessions.
options(httr_oauth_cache=T) 
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

ks <- getUser("keysight")
location(ks)

# get follower data
ks.followers.id <- ks$getFollowers(retryOnRateLimit=10)
ks.followers.df <- rbindlist(lapply(ks.followers.id,as.data.frame))

# look at the locations of 10 followers
head(ks.followers.df$location, 10)

# remove accounts that do not have a location
ks.followers.df <- subset(ks.followers.df, location!="")

# clean the data for use with google map api
ks.followers.df$location <- gsub("%", " ",ks.followers.df$location)

# install modified geocode
#Install key package helpers:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
#Install modified version of the geocode function
#(that now includes the api_key parameter):
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")

# specify to google maps api what is needed
geocode_apply <- function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyDYW8x-KxqzDad1UnKmbJNZTKCxLlq9iZI")
}

# return list of followers that were geocoded
geocode_results <- sapply(ks.followers.df$location, geocode_apply, simplify = F)

# return length of followers with geocode values
length(geocode_results)

#  remove followers with geocode status that is not ok
condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results <- geocode_results[condition_a]

# remove users with ambiguous locations... no more than 1 match
condition_b <- lapply(geocode_results, lapply, length)
condition_b2 <- sapply(condition_b, function(x) x["results"]=="1")
geocode_results <- geocode_results[condition_b2]
length(geocode_results)

# script to clean misformatted data
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")

# send cleaned results to data frame
results_b <- lapply(geocode_results, as.data.frame)

# select only columns needed to map
results_c <- lapply(results_b,function(x) subset(x, select=c("results.formatted_address","results.geometry.location")))

# reshape data to put lattitude and longitude on same line
results_d <- lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                   lat=x[1,"results.geometry.location"],
                                                   lng=x[2,"results.geometry.location"]))
# bind data frame together
results_e <- rbindlist(results_d)

# add column for location strings
results_f <- results_e[,Original_Location:=names(results_d)]

# pull only users from USA
american_results <- subset(results_f,
                         grepl(", USA", results_f$Location)==TRUE)

# only record american results where city data available
# will have 2 commas if location down to city
american_results$commas <- sapply(american_results$Location, function(x)
  length(as.numeric(gregexpr(",", as.character(x))[[1]])))
american_results <- subset(american_results, commas==2)

# drop the "commas" column:
american_results <- subset(american_results, select=-commas)

# show the amount of cleaned american results
nrow(american_results)

# function to install multiple packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.rstudio.com/")
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("maps", "mapproj")
ipak(packages)

# create map of us with state lines
albers_proj <- map("state", proj="albers", param=c(39, 45), 
                 col="#999999", fill=FALSE, bg=NA, lwd=0.2, add=FALSE, resolution=1)
#Add points to it:
points(mapproject(american_results$lng, american_results$lat), col=NA, bg="#00000030", pch=21, cex=1.0)
#Add a title:
mtext("Geography of Keysight Followers USA", side = 3, line = -3.5, outer = T, cex=1.5, font=3)















