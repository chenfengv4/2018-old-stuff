library(TSA)
library(fUnitRoots)
library(forecast)
library(tseries)
library(xlsx)

setwd("C:/secondPillar/cathay/R")
data<- read.csv("Deposits Model Data.csv")
data$Date<- as.Date(as.character(data$Date), format = '%d-%b-%y')
source('functions.R')

data1<- data[,c(2,21,14,15)]
head(data1)

data1$dY<-diffn(data1$Y53,1)

data1$ldX12<-lag(diffn(data1$X12_B,1),1)
data1$dX13<-diffn(data1$X13_B,1)



reg<- arima(data1$dY, order=c(0,0,0), 
            xreg =  cbind(ldX12=data1$ldX12,dX13=data1$dX13), 
            include.mean = T)
reg

reg<-lm(data1$dlnY~data1$X1_B+data1$llnX12+0)
reg

resi<- reg$residuals

resiw<- cbind(data1$Date, data.frame(as.matrix(resi)))
colnames(resiw)<- c('Date', 'Residual')
write.xlsx(resiw, 'residualsMoneyMarketDeposit.xlsx', sheetName = 'MoneyMarketDeposit')
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
