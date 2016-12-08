library(caret)
library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis (augment())
library(ggplot2) # Plotting
library(sqldf) #to reformat the data
library(dplyr) #data prep
library(reshape2) #restructuring data
library(psych) #for descriptive stats
library(pls) #principal component regression

author <- "Jim Turman"

# read in data
quake <- read.csv("all_month.csv")
# pull descriptive data
stats <- sqldf("select depth, mag, nst, gap, dmin, rms, horizontalError, magNst, latitude, longitude from quake")
#descriptive stats 
describe(stats)
describe(quake)

plot(mag~depth,data=stats)
plot(mag~nst,data=stats)
plot(mag~gap,data=stats)
plot(mag~latitude,data=stats)
plot(mag~longitude,data=stats)

mag.lm <- lm(mag ~ ., data = stats)
mag.lm.summary <- summary(mag.lm) # Show the results of the first model
mag.lm.summary

# make predictions
predictions <- predict(mag.lm,stats)
#summarize accuracy of model
rmse <- mean((na.omit(stats$mag-predictions)^2))
print(rmse)

######## Principal Component Regression ############3
fit.pc <- pcr(mag~.,data=stats,validation="CV")
fit.pc.summary <- summary(fit.pc)
predictions.pc <- predict(fit.pc,stats,ncomp=6,na.omit=TRUE)
rmse.pc <- mean((na.omit(stats$mag-predictions.pc)^2))
print(rmse.pc)















