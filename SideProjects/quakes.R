library(dplyr)
library(psych)
library(sqldf)

# read in data
quake <- read.csv("all_month.csv")
# pull descriptive data
stats <- sqldf("select depth, mag, nst, gap, dmin, rms, horizontalError, depthError, magError, magNst from quake")
#descriptive stats 
describe(stats)
plot(stats)

plot(stats$depth)








