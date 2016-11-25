library(Quandl)
library(ggplot2)
library(reshape)
library(psych)

#set authentication credentials
token <- 'mSX5bqBSMyNLZ1TWvvs6' 
Quandl.api_key(token) #authenticate token

cbdax <- Quandl("GOOG/SWX_CBDAX", start_date = "2009-11-01",
                 end_date = "2016-10-31", collapse = "monthly")

bbk <- Quandl("BUNDESBANK/BBK01_WU3141", start_date = "2009-11-01",
                 end_date = "2016-10-31", collapse = "monthly")

df <- data.frame(date = cbdax$Date, cbdax = cbdax$Close, bbk = bbk$Value)
# plot... is it linear?
plot(cbdax~bbk, data = df)

# melt for linear regression
df.m <- melt(df, id.vars = 'date')
head(df.m)
describe(bbk)
describe(cbdax)

ggplot(df.m, aes(x = date, y = value, group = variable, color = variable)) +
  geom_line()

# linear regression
linreg <- summary(lm(cbdax~bbk, data = df))
print(linreg)






















