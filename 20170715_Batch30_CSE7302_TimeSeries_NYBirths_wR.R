#Without Regression on time

rm(list=ls(all=TRUE))
#setwd("C:/Users/Lakshmi/Desktop")

library(forecast)

births <- scan("nybirths.dat")
births
#Else births = read.delim("nybirths.dat", header = F, col.names = c("births"))

#Convert to timeseries object
birthstimeseries <- ts(births,frequency=12,start=c(1946,1))
birthstimeseries

#Plotting
plot.ts(birthstimeseries)

#Decomposition
birthstimeseriescomponents <-  decompose(birthstimeseries)

plot(birthstimeseriescomponents)
birthstimeseriescomponents$seasonal
birthstimeseriescomponents$trend

#ACF and PACF plots
par(mfrow=c(1,3))
plot(birthstimeseries)
acf(birthstimeseries)
pacf(birthstimeseries, lag.max=20)
par(mfrow=c(1,1))

# Split into train and test
births_train = births[1:156]  #(length(births)-12)
births_test = births[157:168]
# records 157 - 168 for testing
births_traints = ts(births_train, frequency = 12,start=c(1946,1))
births_testts = ts(births_test, frequency = 12, start = c(1959,1))

## Holt Winters model
births_HW <-  HoltWinters(births_traints)
births_HW

births_HW$fitted
MAPE_train_HW <- mean(abs(births_traints-births_HW$fitted[,"xhat"])/abs(births_traints))*100

#Forecasting
births_forecast_HW <- 
  forecast.HoltWinters(births_HW,
                       h=12)
births_forecast_HW
MAPE_test_HW <- mean(abs(births_testts - births_forecast_HW$mean)/abs(births_testts))*100

plot.forecast(births_forecast_HW,
              shadecols="oldstyle")


## Auto.Arima
births_autoArima = auto.arima(births_traints)
births_autoArima  #ARIMA(2,1,2)(1,1,1)[12]

births_forecast_autoArima <- forecast.Arima(births_autoArima,h=12)
plot.forecast(births_forecast_autoArima)

MAPE_test_AutoArima <- mean(abs(births_testts - births_forecast_autoArima$mean)/abs(births_testts))*100

## Manual ARIMA model building
#Convert to timeseries object and plot it
plot(births_traints)
#Decompose timeseries data
plot(decompose(births_traints))

#Plot acf and pacf
par(mfrow = c(1,2))
acf(births_traints)
pacf(births_traints)

#Strong seasonality and trend
nsdiffs(births_traints)
ndiffs(births_traints)

#Do a seasonal differencing first
birthsts_sdiff = diff(births_traints,lag = 12, differences = 1)
acf(birthsts_sdiff)
pacf(birthsts_sdiff)

#Do a non-seasonal differencing
birthsts_s_nsdiff = diff(birthsts_sdiff, differences = 1)
acf(birthsts_s_nsdiff)
pacf(birthsts_s_nsdiff)

#Adding a seasonalMA term and non-seasonal MA(3) terms
births_arima1 = Arima(births_traints, order = c(0,1,3), seasonal = c(0,1,1))
births_arima1
par(mfrow = c(1,2))
acf(births_arima1$residuals)
pacf(births_arima1$residuals)
Box.test(births_arima1$residuals, lag = 24, type="Ljung-Box")   #lag = 2*m

#Another model   #Better than auto arima? Adding a seasonal P term
births_arima2 = Arima(births_traints, order = c(0,1,3), seasonal = c(1,1,1))
births_arima2
acf(births_arima2$residuals)
pacf(births_arima2$residuals)
Box.test(births_arima2$residuals, lag = 24, type="Ljung-Box")  

# Forecasting
births_forecast_mArima <- forecast.Arima(births_arima2,h=12)
par(mfrow = c(1,1))
plot.forecast(births_forecast_mArima)

MAPE_test_mArima <- mean(abs(births_testts - births_forecast_mArima$mean)/abs(births_testts))*100

###------------------------end------------------------------------------------------------------##
