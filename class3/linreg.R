### Linear Regression

# Load needed libraries
library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis (augment())
library(ggplot2) # Fancy plotting
# Read data
FuelEff <- read.csv("FuelEfficiency.csv")
# Variable definition
# MPG - Miles per galon
# GPM - Galons per miles
# WT - Weight
# DIS - Dicplacement
# NC - Number of cylinders
# HP - Horsepower
# ACC - acceleration (0-60mph) in seconds
# ET - V-type engine (0) or straight (1)
# Show some data to ensure everything is loaded correctly
head(FuelEff)
# Become familiar with the data
plot(GPM ~ MPG, data = FuelEff)
plot(GPM ~ WT, data = FuelEff)
plot(GPM ~ DIS, data = FuelEff)
plot(GPM ~ NC, data = FuelEff)
plot(GPM ~ HP, data = FuelEff)
plot(GPM ~ ACC, data = FuelEff)
plot(GPM ~ ET, data = FuelEff)
# Remove the MPG rate, since we are going to use GPM (gallons / 100 miles)
# This is because has a linear relationship between the response and thepredictors
FuelEff <- FuelEff[-1] # Remove the first column
head(FuelEff) # Show data
# Regression analyis
## Calculate a regression on all data
FuelEff.m1 <- lm(GPM ~ ., data = FuelEff) # First model using all variables
FuelEff.m1.summary <- summary(FuelEff.m1) # Show the results of the first model
## Getting the confident intervalls
FuelEff.m1.confint <- confint(FuelEff.m1)
## Check with a correlation matrix if predictor variables are themselves related
## Take a look at the correlation between weight and displacement, number of cylinders, and horspower
FuelEff.cor <- cor(FuelEff)
## Calculate all possible regressions
## The result shows the tradeoff for adding more variables.
## As we can see, WT already explains 85% of the regression
x <- FuelEff[,2:7] # Independent variables
y <- FuelEff[,1] # Dependent variables
# Model selection by exhaustive search
FuelEff.out <- summary(regsubsets(x, y, nbest = 2, nvmax = ncol(x)))
FuelEff.regtab <- cbind(FuelEff.out$which,FuelEff.out$rsq, FuelEff.out$adjr2, FuelEff.out$cp) # Stich things together
colnames(FuelEff.regtab) <- c("(Intercept)","WT","DIS","NC", "HP", "ACC", "ET",
                              "R-Sq", "R-Sq (adj)", "Cp") # Add header
print(FuelEff.regtab)
## Create a second model
FuelEff.m2 <- lm(GPM ~ WT, data = FuelEff) # Create a second model with just weight
FuelEff.m2.summary <- summary(FuelEff.m2) # Show results
print(FuelEff.m2.summary)
## Getting the confident intervalls
FuelEff.m2.confint <- confint(FuelEff.m2)
print(FuelEff.m2.confint)
n <- length(FuelEff$GPM) # Get the number of elements
diff <- dim(n) # Set the dimension of the container object
percdiff <- dim(n) # Set the dimension of the container object
# Create a loop to test each combination options of the elements in our data
for (k in 1:n) {
  train1 <- c(1:n)
  # the R expression "train1[train1 != k]" picks from train1 those
  # elements that are different from k and stores those elements in the
  # object train.
  # For k = 1, train consists of elements that are different from 1; that
  # is 2, 3, ..., n.
  train <- train1[train1 != k]
  # Create the linar model for the all but one element
  m1 <- lm(GPM ~ ., data = FuelEff[train,])
  # Predict the missing value based on the model
  pred <- predict(m1, newdat = FuelEff[-train,])
  # What is the real value
  obs <- FuelEff$GPM[-train]
  # Calculate the delta between observed and predicted
  diff[k] <- obs - pred
  # Calculate the relative difference between observed and predicted
  percdiff[k] <- abs(diff[k]) / obs
}
FuelEff.m1.me <- mean(diff) # mean error
FuelEff.m1.rmse <- sqrt(mean(diff**2)) # root mean square error
FuelEff.m1.mape <- 100*(mean(percdiff)) # mean absolute percent error
n <- length(FuelEff$GPM)
diff <- dim(n)
percdiff <- dim(n)
for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 !=k ]
  m2 <- lm(GPM ~ WT, data = FuelEff[train,])
  pred <- predict(m2, newdat = FuelEff[-train,])
  obs <- FuelEff$GPM[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k]) / obs
}
FuelEff.m2.me <- mean(diff)
FuelEff.m2.rmse <- sqrt(mean(diff**2))
FuelEff.m2.mape <- 100*(mean(percdiff))
FuelEff.m1.me # mean error
FuelEff.m1.rmse # root mean square error
FuelEff.m1.mape # mean absolute percent error
FuelEff.m2.me # mean error
FuelEff.m2.rmse # root mean square error
FuelEff.m2.mape # mean absolute percent error
# Check if the assumptions are met...
## Create data frame with residuals
FuelEff.f <- fortify(FuelEff.m1)
## Linearity
### Residual vs Fitted Plot
p1 <- ggplot(FuelEff.f, aes(x = .fitted, y = .resid)) +
  geom_point() +
  stat_smooth(method = "loess") +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")
## Normality
### Normal Q-Q Plot
p2 <- ggplot(FuelEff.f, aes(x = qqnorm(.stdresid)[[1]], y = .stdresid)) +
  geom_point(na.rm = TRUE) +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")
FuelEff.skew <- skewness(FuelEff.f$.resid)
FuelEff.kurt <- kurtosis(FuelEff.f$.resid)
## Equal variance
### Scale-Location Plot
p3 <- ggplot(FuelEff.f, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
  geom_point(na.rm=TRUE) +
  stat_smooth(method = "loess", na.rm = TRUE) +
  xlab("Fitted Value") +
  ylab(expression(sqrt("|Standardized residuals|"))) +
  ggtitle("Scale-Location")
## Independence
# Perform a Durbin-Watson F-test for autocorrelation
FuelEff.dw <- dwtest(m1)
## Outlier influance
### Cook's Distance Histogram
p4 <- ggplot(FuelEff.f, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  ggtitle("Cook's distance")
p5 <- ggplot(FuelEff.f, aes(x =.hat, y = .stdresid)) +
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") +
  ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range = c(1,5)) +theme(legend.position="bottom")
## Save Plots
#ggsave("graphs/linearityAssumption.pdf", p1)
#ggsave("graphs/normalityAssumption.pdf", p2)
#ggsave("graphs/equalVarianceAssumptions.pdf", p3)
#ggsave("graphs/outlierInfluance1Assumptions.pdf", p4)
#ggsave("graphs/outlierInfluance2Assumptions.pdf", p4)

