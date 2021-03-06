#Midterm

student.id <- "0935936"

library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(e1071) # Skewness and Kurtosis
library(broom) # For residual analysis
library(ggplot2) # Plotting
library(sqldf) #to reformat the data
library(dplyr) #data prep
library(reshape2) #restructuring data
library(psych) #for descriptive stats
library(car) # levenes test
#read in data
time <- read.csv("On_Time_On_Time_Performance_2016_8.csv")



############################################# STRUCTURE DATA ####################################################
# pull applicable fields for analysis
delay <- sqldf("select DepDelay,DayOfWeek,UniqueCarrier,Origin,Dest,DestState,DepTime,Cancelled,CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, 
              LateAircraftDelay from time where DepDelayMinutes > 0 and Origin = 'SFO'")
# get data for most common delay 
delay.lr <- sqldf("select DepDelay, DayOfWeek, DepTime, Cancelled, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay
              FROM delay")
delay.carrier <- sqldf("select DepDelay,Carrier from time where DepDelayMinutes > 0 and Origin = 'SFO'")
#find the carrier names
# del.car <- sqldf("select distinct Carrier from time")

################ build data set for anova on carrier delays ####################
########## COMMENTED OUT AND ONLY USED FOR STRUCTURING DATA THEN SAVING AS CSV
# find the carrier names
# del.car <- sqldf("select distinct Carrier from time")
# data for carrier aa
# del.aa <- sqldf("select DepDelay, Carrier from time where Carrier = 'AA' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier as
# del.as <- sqldf("select DepDelay, Carrier from time where Carrier = 'AS' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier B6
# del.b6 <- sqldf("select DepDelay, Carrier from time where Carrier = 'B6' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier DL
# del.dl <- sqldf("select DepDelay, Carrier from time where Carrier = 'DL' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier 00
# del.00 <- sqldf("select DepDelay, Carrier from time where Carrier = 'OO' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier UA
# del.ua <- sqldf("select DepDelay, Carrier from time where Carrier = 'UA' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier VX
# del.vx <- sqldf("select DepDelay, Carrier from time where Carrier = 'VX' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier WN
# del.wn <- sqldf("select DepDelay, Carrier from time where Carrier = 'WN' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier EV
# del.ev <- sqldf("select DepDelay, Carrier from time where Carrier = 'EV' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier F9
# del.f9 <- sqldf("select DepDelay, Carrier from time where Carrier = 'F9' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier HA
# del.ha <- sqldf("select DepDelay, Carrier from time where Carrier = 'HA' and DepDelay != 'NA' and DepDelay > 0")
# data for carrier NK
# del.nk <- sqldf("select DepDelay, Carrier from time where Carrier = 'NK' and DepDelay != 'NA' and DepDelay > 0")

############### create data set for ANOVA ######################

# take random sample of 1000 data points because of different number of observations in carrier delays
# aa.sam <- del.aa[sample(1:nrow(del.aa),1000,replace=FALSE),][1]
# as.sam <- del.as[sample(1:nrow(del.as),1000,replace=FALSE),][1]
# b6.sam <- del.b6[sample(1:nrow(del.b6),1000,replace=FALSE),][1]
# dl.sam <- del.dl[sample(1:nrow(del.dl),1000,replace=FALSE),][1]
# oo.sam <- del.00[sample(1:nrow(del.00),1000,replace=FALSE),][1]
# ua.sam <- del.ua[sample(1:nrow(del.ua),1000,replace=FALSE),][1]
# vx.sam <- del.vx[sample(1:nrow(del.vx),1000,replace=FALSE),][1]
# wn.sam <- del.wn[sample(1:nrow(del.wn),1000,replace=FALSE),][1]
# ev.sam <- del.ev[sample(1:nrow(del.ev),1000,replace=FALSE),][1]
# f9.sam <- del.f9[sample(1:nrow(del.f9),1000,replace=FALSE),][1]
# ha.sam <- del.ha[sample(1:nrow(del.ha),1000,replace=FALSE),][1]
# nk.sam <- del.nk[sample(1:nrow(del.nk),1000,replace=FALSE),][1]

# build the data frame for ANOVA and save to CSV data type

# carrier.aov <- data.frame(aa.sam,as.sam,b6.sam,dl.sam,oo.sam,ua.sam,vx.sam,wn.sam,ev.sam,f9.sam,ha.sam,nk.sam)
# colnames(carrier.aov) <- c("AA","AS","B6","DL","OO","UA","VX","WN","EV","F9","HA","NK")
# rownames(carrier.aov) <- NULL

# because of the random sampling the data frame being used will be saved and the \
# pieces of code performing the random sample will be commented out in order to keep analysis uniform
# SAVE THE DATA CREATED IN CSV
#write.csv(carrier.aov, file = "DelayByCarrier")

# load in data for ANOVA
carrier.delay <- read.csv("DelayByCarrier")
head(carrier.delay)

# assign columns to vectors
aa <- c(carrier.delay$AA)
as <- c(carrier.delay$AS)
b6 <- c(carrier.delay$B6)
dl <- c(carrier.delay$DL)
oo <- c(carrier.delay$OO)
ua <- c(carrier.delay$UA)
vx <- c(carrier.delay$VX)
wn <- c(carrier.delay$WN)
ev <- c(carrier.delay$EV)
f9 <- c(carrier.delay$F9)
ha <- c(carrier.delay$HA)
nk <- c(carrier.delay$NK)

# format data into groups
total <- c(aa,as,b6,dl,oo,ua,vx,wn,ev,f9,ha,nk)
n <- rep(1000,12)
group <- rep(c('AA','AS','B6','DL','OO','UA','VX','WN','EV','F9','HA','NK'),n)

# descriptive stats of data set for anova
tmp = function(x) c(sum=sum(x), mean = mean(x), median=median(x), mode = mode(x), var=var(x), sd = sd(x),
                    n = length(x), kurtosis=kurtosis(x),skew = skewness(x), max = max(x), min = min(x), range = max(x)-min(x), 
                    quartiles = quantile(x),iqr =IQR(x))

tapply(total,group,tmp)
# qqnorm data is not normally distributed... higher chance of type 1 error
qqnorm(total)

# begin anova
# ANOVA
data <- data.frame(total=total, group = factor(group))
fit <- lm(total~group, data)
carrier.delay.aov <- aov(fit)
carrier.delay.aov.summary <- summary(carrier.delay.aov)
print(carrier.delay.aov.summary)

# Levene's Test
carrier.lev <- leveneTest(total~group,data=data)
print(carrier.lev)

# tukey-kramer
carrier.tukey <- TukeyHSD(carrier.delay.aov)
print(carrier.tukey)
plot(carrier.tukey)

###################################### descriptive stats for cause of flight delay########################################3###############
describe(delay.lr)

# number of records for each delay type
    #number of carrier delays
count.cd = 0
for (i in delay$CarrierDelay){
  if (i != 0 && !is.na(i)){
    count.cd <- count.cd + 1
  }
}
cat("Number of carrier delays: ",count.cd)

    #number of weather delays
count.wd = 0
for (i in delay$WeatherDelay){
  if (i != 0 && !is.na(i)){
    count.wd <- count.wd + 1
  }
}
cat("Number of weather delays: ",count.wd)

  # number of NAS delays
count.nd = 0
for (i in delay$NASDelay){
  if (i != 0 && !is.na(i)){
    count.nd <- count.nd + 1
  }
}
cat("Number of NAS delays: ",count.nd)
  
  # number of security delays
count.sd = 0
for (i in delay$SecurityDelay){
  if (i != 0 && !is.na(i)){
    count.sd <- count.sd + 1
  }
}
cat("Number of security delays: ",count.sd)

# number of late aircraft delays
count.ad = 0
for (i in delay$LateAircraftDelay){
  if (i != 0 && !is.na(i)){
    count.ad <- count.ad + 1
  }
}
cat("Number of aircraft delays: ",count.ad)


################################### difference in delay between carriers at SFO #########################################
# build data set for the model 
delay.log <- sqldf("select ArrDelay, case when Carrier = 'AA' then 1
                                    when Carrier = 'AS' then 2
                                    when Carrier = 'B6' then 3
                                    when Carrier = 'DL' then 4
                                    when Carrier = 'UA' then 5
                                    when Carrier = 'OO' then 6
                                    when Carrier = 'VX' then 7
                                    when Carrier = 'WN' then 8
                                    when Carrier = 'F9' then 9
                                    when Carrier = 'HA' then 10
                                    End as Carrier, DepDelay, TaxiOut, Distance, AirTime, 
                                    CarrierDelay, WeatherDelay,
                                    NASDelay, SecurityDelay,LateAircraftDelay
                    from time where Origin = 'SFO'")

# change all NA to 0 then write data to file for analysis
# comment out steps to build data frame after file created
delay.log[is.na(delay.log)] <- 0
DelayModel <- delay.log
rownames(DelayModel) <- NULL
write.csv(DelayModel, file = "DelayModelFields")


# read in formatted data for 
pred.mod <- read.csv("DelayModelFields")
model.dat <- pred.mod[-1]
#model.dat$ArrDelay <- as.factor(model.dat$ArrDelay)

#model selection by exhaustive search
x <- model.dat[,2:11]
y <- model.dat[,1]
model.out <- summary(regsubsets(x, y, nbest = 1, nvmax = ncol(x),force.in = NULL, force.out = NULL, method = "exhaustive"))
model.regtab <- cbind(model.out$which,model.out$rsq, model.out$adjr2, model.out$cp) # Stich things together
colnames(model.regtab) <- c("(Intercept)","Carrier","DepDelay","TaxiOut", "Distance", "AirTime", "CarrierDelay","WeatherDelay","NASDelay",
                            "SecurityDelay","LateAircraftDelay", "R-Sq", "R-Sq (adj)", "Cp") # Add header
print(model.regtab)


#linear regression
delay.lr <- lm(ArrDelay ~ ., data = model.dat)
delay.lr.summary <- summary(delay.lr) 
print(delay.lr.summary) # Show results

# logistic regression
#delay_fit <- glm(ArrDelay~., family=binomial(link='logit'), data=model.dat)
# print(summary(delay_fit))
#print(anova(delay_fit, test="Chisq"))


#plot results
#plot(delay_fit)

## Linearity
### Residual vs Fitted Plot
plot1 <- ggplot(delay.lr, aes(x = .fitted, y = .resid)) +
  geom_point() +
  stat_smooth(method = "loess") +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")
plot2 <- ggplot(delay.lr, aes(x = qqnorm(.stdresid)[[1]], y = .stdresid)) +
  geom_point(na.rm = TRUE) +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")
delay.skew <- skewness(delay.lr$.resid)
delay.kurt <- kurtosis(delay.lr$.resid)
## Equal variance
### Scale-Location Plot
plot3 <- ggplot(delay.lr, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
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
plot4 <- ggplot(delay.lr, aes(x =.hat, y = .stdresid)) +
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") +
  ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range = c(1,5)) +theme(legend.position="bottom")

print(plot1)
print(plot2)
print(plot3)
print(plot4)

## Save Plots
ggsave("linearityAssumption.pdf", plot1)
ggsave("normalityAssumption.pdf", plot2)
ggsave("equalVarianceAssumptions.pdf", plot3)
ggsave("outlierInfluance1Assumptions.pdf", plot4)





















