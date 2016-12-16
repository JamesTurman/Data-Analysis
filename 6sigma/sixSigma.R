# load libraries
library(dplyr)
library(ggplot2)
library(XLConnect)
library(qcc)

set.seed(100)
# create dummy data
diameters <- as.data.frame(replicate(4,rnorm(10,mean=1.315,sd=0.005)))

#range and mean values
diameters <- diameters %>%
  subset(select=1:4) %>%
  mutate(max= do.call(pmax,(.)),
         min = do.call(pmin,(.)),
         mean = rowMeans(.),
         range = do.call(pmax,(.))-do.call(pmin,(.)))
diameters$x <- c(1:10)

# chart
# calc upper and lower control limits
diameters.ucl <- mean(diameters$range)+1.5*sd(diameters$range)
diameters.lcl <- mean(diameters$range)-1.5*sd(diameters$range)

#plot
pr <- ggplot(diameters, aes(x=x,y=range))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = diameters.lcl,color='red',linetype='dashed')+
  geom_hline(yintercept = diameters.ucl,color='red',linetype='dashed')+
  geom_hline(yintercept = mean(diameters$range),color='blue',linetype='dashed')

#x bar chart
# calc upper and lower limits
diametersX.ucl <- mean(diameters$mean) +3*sd(diameters$mean)
diametersX.lcl <- mean(diameters$mean) -3*sd(diameters$mean)

#plot 
px <- ggplot(diameters, aes(x=x,y=mean))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept=diametersX.lcl,color="red",linetype='dashed')+
  geom_hline(yintercept=diametersX.ucl,color="red",linetype='dashed')+
  geom_hline(yintercept = mean(diameters$mean),color="blue",linetype='dashed')

#xbar with only one sample
bank <- readWorksheetFromFile("Bank2.xls",sheet=1)

#add time values
bank$x <- c(1:15)

#calc upper and lower limits
bank.ucl <- mean(bank$Waiting.Time)+3*sd(bank$Waiting.Time)
bank.lcl <- mean(bank$Waiting.Time)-3*sd(bank$Waiting.Time)

p <- ggplot(bank,aes(x=x,y=Waiting.Time))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = bank.lcl,color='red',linetype='dashed')+
  geom_hline(yintercept = bank.ucl,color='red',linetype='dashed')+
  geom_hline(yintercept = mean(bank$Waiting.Time),color='blue',linetype='dashed')

