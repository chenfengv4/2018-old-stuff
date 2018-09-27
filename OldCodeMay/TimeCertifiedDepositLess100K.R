library(TSA)
library(fUnitRoots)
library(forecast)
library(tseries)
library(xlsx)

setwd("C:/secondPillar/cathay/R")
data<- read.csv("Deposits Model Data.csv")
data$Date<- as.Date(as.character(data$Date), format = '%d-%b-%y')
source('functions.R')

data1<- data[,c(2,23,4,7,14)]
head(data1)

data1$lnY<- log(data1$Y55)
#data1$ddlnY<-data1$lnY-2*lag(data1$lnY,1)+lag(data1$lnY,2)
data1$dlnY<-diffn(data1$lnY,1)

data1$l2dX2<-lag(diffn(data1$X2_B,1),2)

data1$l5dX5<-lag(diffn(data1$X5_B,1),5)

data1$lnl5X12<-log(lag(data1$X12_B,5))
#data1$lnl5X12<-lag(log(data1$X12_B),5)
#data1$dlnX12<-diffn(lag(log(data1$X12_B), 5),1)

reg<- arima(data1$dlnY, order=c(0,0,0), 
            xreg =  cbind(l2dX2=data1$l2dX2, l5dX5=data1$l5dX5, lnl5X12=data1$lnl5X12), 
            include.mean = T)
reg

reg<-lm(data1$dlnY~data1$lX4 + data1$X14DD + data1$l5D5+0)
reg

resi<- reg$residuals

resiw<- cbind(data1$Date, data.frame(as.matrix(resi)))
colnames(resiw)<- c('Date', 'Residual')
write.xlsx(resiw, 'residualsTimeDepositLess100.xlsx', sheetName = 'TimeDepositLess100')
resi<- resi[!is.na(resi)]
## 2: acf and pacf test

# you can change the main here (plot title)
acf(resi, plot=T, main='ACF Test of Model Residual', na.action = na.pass)
pacf(resi, plot=T, main='PACF Test of Model Residual',  na.action = na.pass)

## 3: histogram and so on

norms<- rnorm(10000, 0, sd(resi))
par(mfrow=c(2,2))

hist(resi, prob=TRUE, main="Histogram Plot", xlab=NA)
lines(density(resi)$x, (density(resi)$y), col="red")
legend("topright", c('Frequency', 'Normal'), pch=c(0,NA), lty = c(NA, 1), col=c('black', 'red'))


qqnorm(resi)
qqline(resi, col='red', lwd=1.5)

plot(density(resi), main='Kernal Density Estimation (KDE)')
lines(density(norms), col='red')
legend("topright", c('Residual', 'Normal'), lty = c(1, 1), col=c('black', 'red'))


plot(ecdf(resi))
lines(ecdf(norms), col='red')
legend("bottomright", c('Residual', 'Normal'), lty = c(1, 1), col=c('black', 'red'))

par(mfrow=c(1,1))

## normality Test 
normalitytest(resi)

## white noise test (Ljung-Box)
whitenoise(resi, maxlag = 6, fitdf = 0)


## heteroscedasticity
archtesttable(resi, 6)

## stationary

stationary(resi)
