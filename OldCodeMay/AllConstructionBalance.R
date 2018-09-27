library(TSA)
library(fUnitRoots)
library(forecast)
library(tseries)
library(xlsx)
setwd("C:/secondPillar/cathay/R")
data<- read.csv("Balances Model Data.csv")
data$Date <- as.Date(as.character(data$Date), format = '%d-%b-%y')
source('functions.R')

data1<- data[,c(2,23,4,18)]
head(data1)
#data1$Y= NA
#data1$dY <-diffn(data1$Y13,1)



data1$ddY<-data1$Y13-2*lag(data1$Y13,1)+ lag(data1$Y13,2)



data1$dX2<-diffn(data1$X2_B,1)
data1$l2dX16<-lag(diffn(data1$X16_B,1), 2)

#data1<-data1[8:64,]
reg<- arima(data1$ddY, order=c(1,0,0), 
            xreg =   cbind(dX2=data1$dX2,l2dX16=data1$l2dX16), 
            include.mean = F)
reg

plot(data1$dY, fitted(reg))
abline( a=0, b=1)


reg<- auto.arima(data1$dY, xreg =   cbind(dX2=data1$dX2,l2dX16=data1$l2dX16), allowmean = F)



reg<-lm(data1$dY~data1$dX2+data1$l2dX16)
reg
resi<- reg$residuals
f<- fitted(reg)
plot(data1$dY, f)


plot(data1$DATE, f)
points(data1$DATE, data1$dY, col='red', pch=2)


lines(data1$DATE, data1$dY, col='red')
lines(data1$DATE, f, col='black')


plot(data1$dY, f)
# remove outliner
data2<- data1[-c(22, 26),]

reg<- arima(data2$dY, order=c(1,0,0), 
            xreg =   cbind(dX2=data2$dX2,l2dX16=data2$l2dX16), 
            include.mean = F)

reg
resi<- reg$residuals
f<- fitted(reg)
plot(data2$DATE, f)
points(data2$DATE, data2$dY, col='red', pch=2)


lines(data2$DATE, data2$dY, col='red')
lines(data2$DATE, f, col='black')
plot(data2$dY, f)

reg<-lm(data1$dY~data1$dX2 +data1$l2dX16+0)

resi<- reg$residuals


# Write residual to excel

# change sheetname here

resiw<- cbind(data1$Date, data.frame(as.matrix(resi)))
colnames(resiw)<- c('Date', 'Residual')
write.xlsx(resiw, 'residualsAllConstructionBalanceTRUEMODEL.xlsx', sheetName = 'AllConstructionBalance')
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

