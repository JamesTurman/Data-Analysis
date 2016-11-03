#### for tomorrow only use the variables that will be needed to predict 
## create linreg with those variables and give significance
## attempt to create prediction with the given values in the hw

## HW3 Linear Regression to predict price of car
student.ID <- "0935936"

## read the data into a variable
toy <- read.csv("ToyotaCorolla.csv")

library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis (augment())
library(ggplot2) # Plotting
library(sqldf) #to reformat the data
library(dplyr) #data prep
library(reshape2) #restructuring data
library(psych) #for descriptive stats
# get an idea of the data
head(toy)
#KM == number of kilometers on the car
#HP == horsepower
#MetColor == 1 if paint is metallic else 0
#CC == cubic centimeter of engine displacement
# Automatic == transmission type 1 if automatic else 0
#reformat the fuel type column to numeric
#if fuel type = diesel then 0
#if fuel type = petrol then 1
#if fuel type = CNG then 2
toy.r <- sqldf("SELECT Price, Age, KM, CASE WHEN FuelType = 'Diesel' THEN 0 WHEN FuelType = 'Petrol'
               THEN 1 WHEN FuelType = 'CNG' THEN 2 END AS FuelType, HP, MetColor, Automatic, CC, Doors, Weight FROM toy")

############# DESCRIPTIVE STATS ######################
describe(toy.r)

#inspect new file with all numeric
head(toy.r)

## become familiar with the data
plot(Price ~ Age, data = toy.r)
plot(Price ~ KM, data = toy.r)
plot(Price ~ FuelType, data = toy.r)
plot(Price ~ HP, data = toy.r)
plot(Price ~ MetColor, data = toy.r)
plot(Price ~ Automatic, data = toy.r)
plot(Price ~ CC, data = toy.r)
plot(Price ~ Doors, data = toy.r)
plot(Price ~ Weight, data = toy.r)

## begin the regression analysis using all variables
toy.p1 <- lm(Price ~ ., data = toy.r)
toy.p1.summary <- summary(toy.p1) # Show the results of the first model
toy.p1.summary
#shows hypothesis test if P value < .05 then variable is significant contributor to model

# get confidence intervals
toy.p1.confint <- confint(toy.p1)
toy.p1.confint
## Check with a correlation matrix if predictor variables are themselves related
toy.cor <- cor(toy.r)
toy.cor

x <- toy.r[,2:10] # Independent variables
y <- toy.r[,1] # Dependent variables
# Model selection by exhaustive search
toy.out <- summary(regsubsets(x, y, nbest = 1, nvmax = ncol(x),force.in = NULL, force.out = NULL, method = "exhaustive"))
toy.regtab <- cbind(toy.out$which,toy.out$rsq, toy.out$adjr2, toy.out$cp) # Stich things together
colnames(toy.regtab) <- c("(Intercept)","Age","KM","FuelType", "HP", "MetColor", "Automatic","CC","Doors","Weight",
                              "R-Sq", "R-Sq (adj)", "Cp") # Add header
print(toy.regtab)

#optimal model is price age km fueltype hp automatic cc weight
#create a second model with "Price","Age","KM", "FuelType","HP","Automatic", "CC", "Weight"
#model creates the highest adjusted r2 so it is the optimal solution
toy.2 <- data.frame(toy.r$Price,toy.r$Age, toy.r$KM, toy.r$FuelType,toy.r$HP, toy.r$Automatic, toy.r$CC,toy.r$Weight)
colnames(toy.2) <- c("Price","Age", "KM", "FuelType","HP","Automatic", "CC", "Weight")

#show the optimal model 
toy.p2 <- lm(Price ~ ., data = toy.2)
toy.p2.summary <- summary(toy.p2) 
print(toy.p2.summary) # Show results

############ PREDICT THE PRICE OF CAR ##############
#model for predicting the price of the car with given data points NOT OPTIMAL
toy.3 <- data.frame(toy.r$Price,toy.r$Age,toy.r$FuelType,toy.r$HP,toy.r$MetColor,toy.r$Automatic,toy.r$CC,toy.r$Doors)
colnames(toy.3) <- c("Price","Age","FuelType","HP","MetColor","Automatic","CC","Doors")
#run linear regression and print
toy.p3 <- lm(Price ~., data=toy.3)
toy.p3.summary <- summary(toy.p3)
print(toy.p3.summary)
#predict values given in hw
price.pred <- data.frame(Age = 12, FuelType = 1, HP = 185, MetColor = 1, Automatic = 0, 
                         CC = 2000, Doors = 4)
predict(toy.p3,price.pred)


###################### CONFIDENCE INTERVALS#########################
toy.p3.confint <- confint(toy.p3)
print(toy.p3.confint)
n <- length(toy.r$Price) # Get the number of elements
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
  m1 <- lm(Price ~ ., data = toy.3[train,])
  # Predict the missing value based on the model
  pred <- predict(m1, newdat = toy.3[-train,])
  # What is the real value
  obs <- toy.3$Price[-train]
  # Calculate the delta between observed and predicted
  diff[k] <- obs - pred
  # Calculate the relative difference between observed and predicted
  percdiff[k] <- abs(diff[k]) / obs
}
toy.3.me <- mean(diff) # mean error
toy.3.rmse <- sqrt(mean(diff**2)) # root mean square error
toy.3.mape <- 100*(mean(percdiff)) # mean absolute percent error
toy.3.me
toy.3.rmse
toy.3.mape

toy.g <- fortify(toy.p1)
## Linearity
### Residual vs Fitted Plot
plot1 <- ggplot(toy.g, aes(x = .fitted, y = .resid)) +
  geom_point() +
  stat_smooth(method = "loess") +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")
plot2 <- ggplot(toy.g, aes(x = qqnorm(.stdresid)[[1]], y = .stdresid)) +
  geom_point(na.rm = TRUE) +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")
FuelEff.skew <- skewness(toy.g$.resid)
FuelEff.kurt <- kurtosis(toy.g$.resid)
## Equal variance
### Scale-Location Plot
plot3 <- ggplot(toy.g, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
  geom_point(na.rm=TRUE) +
  stat_smooth(method = "loess", na.rm = TRUE) +
  xlab("Fitted Value") +
  ylab(expression(sqrt("|Standardized residuals|"))) +
  ggtitle("Scale-Location")
## Independence
# Perform a Durbin-Watson F-test for autocorrelation
##toy.g.dw <- dwtest(m1)
## Outlier influance
### Cook's Distance Histogram
plot4 <- ggplot(toy.g, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  ggtitle("Cook's distance")
plot5 <- ggplot(toy.g, aes(x =.hat, y = .stdresid)) +
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") +
  ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range = c(1,5)) +theme(legend.position="bottom")
## Save Plots
ggsave("linearityAssumption.pdf", plot1)
ggsave("normalityAssumption.pdf", plot2)
ggsave("equalVarianceAssumptions.pdf", plot3)
ggsave("outlierInfluance1Assumptions.pdf", plot4)
ggsave("outlierInfluance2Assumptions.pdf", plot5)






















