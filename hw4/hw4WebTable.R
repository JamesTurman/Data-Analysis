######### Web Scraping ######### 


# Load needded libraries
library(XML) # For scraping the HTML tables
library(dplyr) # Data manipulation library
library(lubridate) # For data manipulation
library(stringi) # Provides a host of string opperations
library(reshape) # melt data
library(psych) #descriptive stats

# Harvest data
url.mlb <- "http://www.espn.com/mlb/standings"

## Get the raw HTML data
tables <- readHTMLTable(url.mlb, header = FALSE, 
                        stringsAsFactors = FALSE)

standings.df <- data.frame(tables)

#separate american league teams
al <- data.frame(standings.df$NULL.V1,standings.df$NULL.V2,standings.df$NULL.V3,standings.df$NULL.V4,
                 standings.df$NULL.V5,standings.df$NULL.V6,standings.df$NULL.V7,standings.df$NULL.V8,
                 standings.df$NULL.V9,standings.df$NULL.V10,standings.df$NULL.V11,standings.df$NULL.V12,
                 standings.df$NULL.V13)
#assign column names
colnames(al) <- c("Team","Wins","Losses","PCT", "Games Back", "Home", "Road","F","RA","Diff",
                          "STRK", "L10", "POFF")
#make numbers numeric
al[, c(2:5,8,9,12)] <- sapply(al[, c(2:5,8,9,12)], as.factor)

# separate national league teams
nl <- data.frame(standings.df$NULL.V1.1,standings.df$NULL.V2.1,standings.df$NULL.V3.1,standings.df$NULL.V4.1,
                 standings.df$NULL.V5.1,standings.df$NULL.V6.1,standings.df$NULL.V7.1,standings.df$NULL.V8.1,
                 standings.df$NULL.V9.1,standings.df$NULL.V10.1,standings.df$NULL.V11.1,standings.df$NULL.V12.1,
                 standings.df$NULL.V13.1)
#assign column names
colnames(nl) <- c("Team","Wins","Losses","PCT", "Games Back", "Home", "Road","F","RA","Diff",
                  "STRK", "L10", "POFF")
#make numbers numeric
nl[, c(2:5,8,9,12)] <- sapply(nl[, c(2:5,8,9,12)], as.factor)

#descriptive stats al
describe(al)
describe(nl)







































