#### for tomorrow only use the variables that will be needed to predict 
## create linreg with those variables and give significance
## attempt to create prediction with the given values in the hw

## HW3 Linear Regression to predict price of car
author <- "Jim Turman"

## read the data into a variable
toy <- read.csv("ToyotaCorolla.csv")

library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis (augment())
library(ggplot2) # Plotting
library(sqldf) #to reformat the data

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

#create a second model with Age, KM, Weight from correlation matrix
#all have at least a moderate correlation to price whether negative or positive
toy.2 <- data.frame(toy.r$Price,toy.r$Age,toy.r$KM,toy.r$HP,toy.r$Weight)
colnames(toy.2) <- c("Price","Age","KM","HP","Weight")

toy.p2 <- lm(Price ~ ., data = toy.2) # Create a second model with just weight
toy.p2.summary <- summary(toy.p2) # Show results
print(toy.p2.summary)

#does not represent a better r squared or adjusted r squared than the original model
#use toy.p1 as model

## Getting the confident intervalls
FuelEff.m2.confint <- confint(FuelEff.m2)
print(FuelEff.m2.confint)
n <- length(FuelEff$GPM) # Get the number of elements
diff <- dim(n) # Set the dimension of the container object
percdiff <- dim(n) # Set the dimension of the container object








