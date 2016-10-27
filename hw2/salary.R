setwd("C:/Users/jaturman/Desktop/data analysis/ops804")
sal <- read.csv("salary.csv")

library(reshape2)
library(dplyr)
library(e1071)
library(car)
library(ggplot2)
library(reshape)

#assign columns to vectors by sport
nfl <- c(sal$nfl)
mlb <- c(sal$mlb)
nba <- c(sal$nba)

#format data into groups
tot <- c(nfl,mlb,nba)
n <- rep(15,3)
group <- rep(c('nfl','mlb','nba'),n)

#summary stats
tmp = function(x) c(sum=sum(x), mean = mean(x), median=median(x), mode = mode(x), var=var(x), sd = sd(x),
                    n = length(x), kurtosis=kurtosis(x),skew = skewness(x), max = max(x), min = min(x), range = max(x)-min(x), 
                    quartiles = quantile(x),iqr =IQR(x))
tapply(tot,group,tmp)

#shapiro test for normality
setwd("C:/Users/jaturman/Desktop/data analysis/ops804")
sal <- read.csv("salary.csv")

library(reshape2)
library(dplyr)
library(e1071)
library(car)
library(ggplot2)
library(reshape)

#assign columns to vectors by sport
nfl <- c(sal$nfl)
mlb <- c(sal$mlb)
nba <- c(sal$nba)

#format data into groups
tot <- c(nfl,mlb,nba)
n <- rep(15,3)
group <- rep(c('nfl','mlb','nba'),n)

#summary stats
tmp = function(x) c(sum=sum(x), mean = mean(x), median=median(x), mode = mode(x), var=var(x), sd = sd(x),
                    n = length(x), kurtosis=kurtosis(x),skew = skewness(x), max = max(x), min = min(x), range = max(x)-min(x), 
                    quartiles = quantile(x),iqr =IQR(x))
tapply(tot,group,tmp)

#qq plot to verify results of shapiro test
shapiro.test(tot)

#anova
data <- data.frame(tot=tot, group = factor(group))
fit <- lm(tot~group, data)
sal.aov <- aov(fit)
sal.aov.summary <- summary(sal.aov)
print(sal.aov.summary)

#perform Tukey-Kramer
sal.tukey <- TukeyHSD(sal.aov)
print(sal.tukey)

#perform Levene's test
sal.lev <- leveneTest(tot~group, data = data)
print(sal.lev)
#qq plot to verify results of shapiro test
qqnorm(tot)

#anova
data <- data.frame(tot=tot, group = factor(group))
fit <- lm(tot~group, data)
sal.aov <- aov(fit)
sal.aov.summary <- summary(sal.aov)
print(sal.aov.summary)

#perform Tukey-Kramer
sal.tukey <- TukeyHSD(sal.aov)
print(sal.tukey)
plot(sal.tukey)

#perform Levene's test
sal.lev <- leveneTest(tot~group, data = data)
print(sal.lev)