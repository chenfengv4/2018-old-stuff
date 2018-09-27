library(TSA)
library(fUnitRoots)
library(forecast)
library(tseries)
library(xlsx)

setwd("C:/secondPillar/cathay/R")
data<- read.csv("Balances Model Data.csv")
data$Date<- as.Date(as.character(data$Date), format = '%d-%m-%y')
source('functions.R')


#### steps 1: manully clean the data (lag or diff)
data1<- data[,c(2,19,8,15)]
head(data1)

data1$dY <-diffn(data1$Y5,1)
data1$lX6<-lag(data1$X6_B,1)

data1$l1d1X13<-lag(diffn(data1$X13_B,1),1)



#### ARIMAX ####
#reg<- arimax(data1$Y, order=c(1,0,0), 
             xtransf = cbind(l5X1=data1$l5X1,dl2X14=data1$dl2X14, l1d1X12=data1$l1d1X12), 
             transfer = list(c(0,0),c(0,0),c(0,5)), include.mean = T)
#reg

#reg<- auto.arima(data1$dY, xreg= cbind(l4X6=data1$l4X6,dX15=data1$dX15), 
#                 allowmean = F, approximation = F, stationary = T,
#                 ic='aicc')

reg<- arima(data1$dY, xreg= cbind(lX6=data1$lX6,l1d1X13=data1$l1d1X13),
            include.mean = F, order=c(2,0,0))
reg
# save residual
resi<- reg$residuals
# Go to Result writing part



#### 
#### Result writing ####
####

## 1: Write residual to excel

# change sheetname here

resiw<- cbind(data1$Date, data.frame(as.matrix(resi)))
colnames(resiw)<- c('Date', 'Residual')
write.xlsx(resiw, 'residualsHELOC.xlsx', sheetName = 'HELOC')
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






