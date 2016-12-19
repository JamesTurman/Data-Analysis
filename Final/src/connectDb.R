library(dplyr)
library(DBI)
library(RPostgreSQL)

drv <- dbDriver("PostgreSQL")

source("src/dbSettings.R")

flight.db<-dbConnect(drv,
                     host = host,
                     port = port,
                     dbname=dbname,
                     user=user,
                     password=pwd)
