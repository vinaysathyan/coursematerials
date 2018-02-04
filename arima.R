# Directories
setwd("C:/Users/michaeljgrogan/Documents/1 Documents/Computing/Data Science Folders/Datasets")
mydata<- read.csv("C:/Users/michaeljgrogan/Documents/1 Documents/Computing/Data Science Folders/Datasets/jnj.csv")
attach(mydata)

#Load libraries
library(MASS)
library(tseries)
library(forecast)

#Plot and convert to ln format
lnstock=log(price[1:96])
lnstock

# ACF, PACF and Dickey-Fuller Test
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)
adf.test(lnstock)

#Time series and auto.arima
pricearima <- ts(lnstock, start = c(2006,09), frequency = 12)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima,type='l')
title('JNJ Price')
exp(lnstock)

#Forecasted Values From ARIMA
forecastedvalues_ln=forecast(fitlnstock,h=26)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastvalues=exp(forecastedvaluesextracted)
finalforecastvalues

#Percentage Error
df<-data.frame(price[96:121],finalforecastvalues)
col_headings<-c("Actual Price","Forecasted Price")
names(df)<-col_headings
attach(df)
percentage_error=((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
percentage_error
mean(percentage_error)

#Ljung-Box
Box.test(fitlnstock$resid, lag=5, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=10, type="Ljung-Box")
Box.test(fitlnstock$resid, lag=15, type="Ljung-Box")