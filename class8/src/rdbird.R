# rudybird files
# find something interesting... pull out the golden nugget
author <- 'Jim Turman'
# load libraries
library(dplyr)
library(ggplot2)
library(XLConnect)
library(astsa)
library(psych)

#read in data
#total cases is sales in total... campaign to sell more rudybirds 
#rudybird is rudybird sales 
#first 30 without promotion rest are with promotion

rb <- readWorksheetFromFile("RudyBird.xls",sheet=1)
#get a sense of the data
plot(rb)
describe(rb)

#subset to separate campaign from non compaign
rb.mark <- as.data.frame(subset(rb,Day >30))
rb.mark <- as.vector(subset(rb.mark[3]))

rb.no <- subset(rb,Day<31)
rb.no <- subset(rb.no[3])
#descriptive stats of groups
describe(rb.mark)
describe(rb.no)
#format data for tests
yes <- unlist(rb.mark, recursive = TRUE, use.names = FALSE)
yes <- c(yes)
no <- unlist(rb.no,recursive = TRUE,use.names = FALSE)
no <- c(no)
#testing homogeneity of variance
rb.varTest <- var.test(yes,no)
#t test of sample means
rb.hypTest <- t.test(yes,no,var.equal = TRUE,paired = FALSE)


