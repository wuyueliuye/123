library(astsa)
energy <- read.csv(file.choose())
date <- as.Date(energy$Datetime)
energy$date <- date
library(tidyr)
energy.upd <- energy %>% separate(date, sep="-", into = c("year", "month", "day"))
energy.upd$time <- format(as.POSIXct(strptime(energy.upd$Datetime,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")
library(dplyr)
energy.cln1 <- energy.upd%>%group_by(year, month)%>%summarise(consumption=mean(COMED_MW))
energy.cln2 <- energy.upd%>%group_by(year, month, day)%>%summarise(consumption=mean(COMED_MW))
energy.cln2$week <-  rep(1:52, each=7, length.out=2772)
energy.cln3 <- energy.cln2%>%group_by(year, week)%>%summarise(consumption=mean(consumption))
e.cln1.ts <- ts(energy.cln1$consumption, start = c(2011, 1), frequency = 12)
e.cln2.ts <- ts(energy.cln2$consumption, start = c(2011, 1), frequency = 365)
e.cln3.ts <- ts(energy.cln3$consumption, start = c(2011, 1), frequency = 52)
fix(energy.cln1)
energy.cln1$temp=c(42.8,	49.5,	61.3,	70.8,	72.8,	86.8,	91.4,	93.4,	80,	68.2,	57.9,	47.6,
                   50.4,	52.5,	64.3,	70.3,	77.9,	84.3,	87.7,	86.5,	80.0,	67.0,	59.7,	51.2,
                   49.1,	52.0,	56.4,	63.0,	72.3,	82.6,	84.5,	87.1,	82.4,	68.2,	53.5,	43.1,
                   45.3,	47.0,	55.1,	66.3,	74.4,	82.4,	83.8,	86.2,	80.3,	71.6,	51.5,	50.1,
                   44.5,	45.7,	56.1,	65.8,	70.9,	82.1,	87.1,	87.3,	82.7,	71.2,	58.7,	53.7,
                   47.0,	55.2,	61.2,	68.1,	72.5,	84.0,	87.4,	85.8,	81.5,	74.1,	63.5,	49.7,
                   51.2,	60.6,	65.7,	69.3,	75.4,	82.5,	86.6,	84.4,	80.6,	69.6,	62.4,	49.7,
                   45.8,	51.1,	63.3,	61.6,	79.0,	85.7,	88.8,	85.2)
energy.cln1$holidays = rep(c(2, 1, 0, 0, 1, 0, 1, 0, 1, 1, 2, 1), length.out=length(energy.cln1$consumption))
temp.cln1.ts <- ts(energy.cln1$temp, start = c(2011, 1), frequency = 12)
hd.cln1.ts <- ts(energy.cln1$holidays, start = c(2011, 1), frequency = 12)


########## Descriptive Analysis and Exploratory Analysis
par(mfrow=c(3, 1))
plot(e.cln2.ts, type = "o")
title("Hourly energy consumption by day")
plot(e.cln3.ts, type = "o")
title("Hourly energy consumption by week")
plot(e.cln1.ts, type = "o")
title("Hourly energy consumption by month")

############ training testing split
training <- energy.cln1[1:(round(0.8*nrow(energy.cln1))),]
testing <- energy.cln1[(round(0.8*nrow(energy.cln1))):nrow(energy.cln1) ,]

e.t.ts <- ts(training$consumption, start = c(2011, 1), frequency = 12)
e.testing.ts <- ts(testing$consumption, start = c(2011, 1), frequency = 12)
temp.t.ts <- ts(training$temp, start = c(2011, 1), frequency = 12)
hd.t.ts <- ts(training$holidays, start = c(2011, 1), frequency = 12)

########## regression with autocorrelated errors
trend  = time(e.t.ts) 
temp   = temp.t.ts - mean(temp.t.ts)
temp2  = temp^2
hd = hd.t.ts
summary(fit <- lm(e.t.ts~trend + temp + temp2+ hd, na.action=NULL))

#### add the autocorrelated errors
plot.ts(resid(fit))
acf2(resid(fit)) # possibly AR(2)
sarima(e.t.ts, 1,0,0, xreg=cbind(trend,temp,temp2, hd) ) # AIC 15.59315
sarima(e.t.ts, 2,0,0, xreg=cbind(trend,temp,temp2, hd) ) # AIC 15.56977 this one
reg1 <- sarima(e.t.ts, 2,0,0, xreg=cbind(trend,temp,temp2, hd) ) 

########## by month
plot(e.t.ts, type = "o")
title("Hourly energy consumption by month")
plot(stl(e.t.ts , s.window = "periodic"))# make decomposition of data. 
le1 <- log(e.t.ts)
plot.ts(le1)
par(mfrow=c(4, 1))
plot(diff(le1, 1))
plot(diff(le1, 2))
plot(diff(le1, 3))
plot(diff(le1, 6))

dle1.0 <- diff(le1, 1)
acf2(dle1.0)
dle1 <- diff(le1, 3)
plot.ts(dle1)
acf2(dle1)
ddle1 <- diff(dle1, 12)
ddle1.0 <- diff(dle1.0,12)
par(mfrow=c(1,1))
plot.ts(ddle1)

par(mfrow=c(2,1))
monthplot(dle1)
monthplot(ddle1)

acf2(ddle1.0) # p,q 0 or 1; P, Q 0, 1 or 2
sarima(ddle1.0, 1,1,1, 0,1,1,12 ) # pass
sarima(ddle1.0, 1,1,1, 0,1,2,12 ) # aic -1.643136 
sarima(ddle1.0, 0,1,1, 0,1,2,12 ) # aic -1.648339 this one
sarima(ddle1.0, 1,1,0, 0,1,2,12 ) # aic -1.405368
sarima(ddle1.0, 0,1,1, 0,1,1,12 ) # aic -1.610868
sarima(ddle1.0, 1,1,1, 0,1,1,12 ) # aic -1.632914
sarima(ddle1.0, 1,1,0, 0,1,1,12 ) # pass
sarima.for(e.t.ts, 18, 0,1,1, 0,1,2, 12 )
line(e.testing.ts)
plot(e.cln1.ts, type='o')

########## model validation and performance evaluation 
library(forecast)
m1 <- sarima.for(e.t.ts, 18, 0,1,1, 0,1,2, 12 )
accuracy(m1$pred, testing$consumption[-1])


########## Spectral Analysis

mvspec(e.cln1.ts, spans=10, log='no')
mvspec(e.cln1.ts, log='no')
SigExtract(e.cln1.ts)
install.packages('ChemoSpec')


########## by week
plot.ts(e.cln3.ts)
plot(e.cln3.ts, type = "o")
title("Hourly energy consumption by week")
plot(stl(e.cln3.ts , s.window = "periodic"))# make decomposition of data. 
le3 <- log(e.cln3.ts)
plot.ts(le3)
dle3.0 <- diff(le3, 1) #this transformation
plot.ts(dle3.0)
acf2(dle3.0)
dle3 <- diff(le3, 3) #pass
plot.ts(dle3) #pass
acf2(dle3) #pass
ddle3 <- diff(dle3, 52) #pass
ddle3.0 <- diff(dle3.0,52)
plot.ts(ddle3.0)

sarima(ddle3.0, 0, 1, 1, 0, 1, 1, 52) # pass 
sarima(ddle3.0, 0, 1, 4, 0, 1, 3, 52) # aic -1.331753 this one
sarima(ddle3.0, 2, 1, 4, 1, 1, 0, 52) # pass
sarima(ddle3.0, 1, 1, 0, 1, 1, 0, 52) # pass
sarima(ddle3.0, 0, 1, 1, 1, 1, 0, 52) # pass
sarima(ddle3.0, 1, 1, 0, 4, 1, 0, 52) # error
sarima(ddle3.0, 0, 1, 1, 4, 1, 0, 52) # pass
sarima(ddle3.0, 0, 1, 4, 0, 1, 4, 52) # aic -1.327269
sarima(ddle3.0, 0, 1, 1, 0, 1, 3, 52) # pass

########## by day PASS
plot.ts(e.cln2.ts, type='o')
plot(stl(e.cln2.ts , s.window = "periodic"))# make decomposition of data. 
le2 <- log(e.cln2.ts)
plot.ts(le2)
dle2 <- diff(le2,1)
plot.ts(dle2)
acf2(dle2, 100)
ddle2 <- diff(dle2, 2)
plot.ts(ddle2)
acf2(ddle2, 365)


########## Spectral Analysis
mvspec(e.cln3.ts, spans=10, log='no')
mvspec(e.cln3.ts, log='no')
SigExtract(e.cln3.ts)

mvspec(e.cln1.ts, spans=10, log='no')
mvspec(e.cln1.ts, log='no')
SigExtract(e.cln1.ts)




e5 <- ts(e3_r$consumption, start = c(2017, 1), frequency = 365)
e6 <- ts(e3_0$consumption, start = c(2011, 1), frequency = 12)
plot.ts(e5)
plot.ts(log(e5))
plot.ts(diff(log(e5), 1))
d1le5 <- diff(log(e5), 1)[-1]
acf2(d1le5)
plot.ts(e6)
plot.ts(diff(e6, 1))
d1e6 <- diff(e6, 1)[-1]
acf2(d1e6) # ineffective ACF
# try log-transformation
le6 <- log(e6)
plot.ts(le6) # unstable due to the mean
# take difference
dle6 <- diff(le6, 1)
plot.ts(dle6) # looks stable
acf2(dle6, 80) # looks seasonal: tails 5 cuts 1, nonseasonal: ineffective acf, tails 4
# take additional difference
ddle6 <- diff(dle6, 3)
plot.ts(ddle6)
acf2(ddle6)

d6e6 <- diff(e6, 6) # diffence 6 lags
dd6e6 <- diff(d6e6, 12)
plot.ts(d6e6) #plot
par(mfrow=c(2, 1))
monthplot(d6e6) # not a good monthly stable, pass
monthplot(dd6e6) # looks good monthly stable
acf2(d6e6) # seansonal: acf tails 1, pacf cuts, nonseasonal: acf tails 2, pacf tails 2 or cuts
acf2(dd6e6) # seasonal: acf tails 2, pacf cuts, nonseasonal: acf tails 2, pacf cuts
sarima(dd6e6, 2, 6, 0, 2, 1, 0, 12)

d4e6 <- diff(e6, 4) # diffence 6 lags
dd4e6 <- diff(d4e6, 12)
plot.ts(d4e6) #plot
par(mfrow=c(2, 1))
monthplot(d4e6) # not a good monthly stable, pass
monthplot(dd4e6) # looks good monthly stable
acf2(d4e6) # seansonal: acf tails 1, pacf cuts, nonseasonal: acf tails 2, pacf tails 2 or cuts
acf2(dd4e6) # seasonal: acf tails 3, pacf cuts, nonseasonal: acf tails 6, pacf cuts


sarima(dd4e6, 6, 4, 0, 3, 1, 0, 12)
sarima(d4e6, 6, 4, 0, 1, 0, 0, 4)


par(mfrow=c(4, 1))
plot.ts(diff(e6, 1))
plot.ts(diff(e6, 2))
plot.ts(diff(e6, 3))
plot.ts(diff(e6, 6))



plot.ts(diff(e6, 1))
acf2(diff(e6, 1))
monthplot(diff(e6, 1))
d1e6 <- diff(e6,1)
dd1e6 <- diff(d1e6, 12)
monthplot(dd1e6)
acf2(dd1e6) #nonseasonal: cuts/tails1, tails 1, seasonal: cuts, tails 2
sarima(dd1e6, 1, 1, 1, 0, 1, 2, 12) # aic 14.22 this one
sarima(dd1e6, 0, 1, 1, 0, 1, 2, 12) # aic 14.25


e6
dde6 <- diff(diff(log(e6)), 12)
monthplot(d1e6)
monthplot(dde6)
plot.ts(dde6)
acf2(dde6, 50) #seasonal tails off 3, tails off 3, nonseasonal cuts 1, tails 2
plot(e6, type = "o")
title("Hourly energy consumption by month")
plot(stl(e6 , s.window = "periodic"))# make decomposition of data. 
plot(stl(dde6 , s.window = "periodic"))
