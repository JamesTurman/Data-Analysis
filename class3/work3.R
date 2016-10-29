#new R file for class 3

library(dplyr)
library(reshape2)

## load data
fueleff <- read.csv("FuelEfficiency.csv")

## restructure
fuel <- fueleff[-1]
head(fuel)

fuel.m1 <- lm(GPM ~ ., data = fuel) #regression
fuel.m2.sum <- summary(fuel.m1) #summary of regression
fuel.m1.confint <- confint(fuel.m1) #confidence interval

##look at correlation matrix
fuel.cor <- cor(fuel)

x<-fuel[,2:7]
y<-fuel[,1]

fuel.out <-summary(regsubsets(x,y,nbest=2,nvmax=ncol(x)))
fuel.regtab <- cbind(fuel.out$which, fuel.out$rsq, fuel.out$adjr2, fuel.out$cp)

colnames(fuel.regtab) <- c("(Intercept)","WT","DIS",
                              "NC", "HP", "ACC", "ET",
                              "R-Sq", "R-Sq (adj)", "Cp") # Add header
print(fuel.regtab)

fuel.m2 <- lm(GPM ~ WT, data = fuel)
fuel.m2.summary <- summary(fuel.m2)
fuel.m2.confint <- confint(fuel.m2)
print(fuel.m2.confint)

n <- length(fuel$GPM)
diff <- dim(n)
percdiff <- dim(n)

for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 != k]
  m1 <- lm(GPM ~ ., data = fuel[train,])
  pred <- predict(m1, newdat = fuel[-train,])
  obs <- fuel$GPM[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k])/obs
}

n <- length(fuel$GPM)
diff <- dim(n)
percdiff <- dim(n)

for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 != k]
  m2 <- lm(GPM ~ ., data = fuel[train,])
  pred <- predict(m2, newdat = fuel[-train,])
  obs <- fuel$GPM[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k])/obs
}

fuel.m1.me <- mean(diff)
fuel.m1.rmse <- sqrt(mean(diff**2))
fuel.m1.mape <- 100*(mean(percdiff))

fuel.m2.me <- mean(diff)
fuel.m2.rmse <- sqrt(mean(diff**2))
fuel.m2.mape <- 100*(mean(percdiff))


fuel.m1.me
fuel.m1.mape
fuel.m1.rmse

fuel.m2.me
fuel.m2.mape
fuel.m2.rmse



