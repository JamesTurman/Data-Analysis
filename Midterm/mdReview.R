#### midterm reviewed ####
author <- "Jim Turman"
student.id <- "00935936"

library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis
library(ggplot2) # Plotting
library(sqldf) #to reformat the data
library(dplyr) #data prep
library(reshape2) #restructuring data
library(psych) #for descriptive stats
library(car) # levenes test
library(data.table) #to load data reads better than the read.csv function

# extract data
unzip("data/On_Time_On_Time_Performance_2016_8.zip")

flights <- fread("On_Time_On_Time_Performance_2016_8.csv")

str(flights.dt)












