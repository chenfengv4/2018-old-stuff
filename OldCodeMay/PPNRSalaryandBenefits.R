library(TSA)
library(fUnitRoots)
library(forecast)
library(tseries)
library(xlsx)
library(arimax)
setwd("C:/secondPillar/cathay/R")
data<- read.csv("Cathay Data.csv")
data$DATE<- as.Date(as.character(data$DATE), format = '%m/%d/%y')
source('functions.R')

#### CASE 1: Bs with independent variables  ####
#### Model 1 Example ####
####

#### steps 1: manully clean the data (lag or diff)
data1<- data[,c(1,51,4,14)]
head(data1)

data1$dY <-diffn(data1$Y71,1)
data1$lX2<-lag(data1$X2_B,1)

data1$l1d1X12<-lag(diffn(data1$X12_B,1),1)
data1
reg<- arimax(data1$dY, order=c(2,0,0), 
             xtransf = cbind(lX2=data1$lX2, l1d1X12=data1$l1d1X12), 
             transfer = list(c(0,0),c(0,0)), include.mean = F)
reg
reg<- arima(data1$dY, xreg= cbind(lX2=data1$lX2, l1d1X12=data1$l1d1X12),
            include.mean = F,order=c(2,0,0))
reg
plot(data1$dY, fitted(reg))
