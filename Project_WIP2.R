library(forecast)			
library(zoo)
library(readxl)
Temp.data <- read_excel("/Users/ningb/OneDrive - purdue.edu/672aba/data//TempData.xlsx")
#View(Temp.data)

# Creating a time series object
Temp.ts <- ts(Temp.data$Temperature, start = c(1981,1), end = c(1990,12), freq = 12)

# Plotting the time series
plot(Temp.ts, main = "Temperature readings for the years 1981-1990", xlab = "Year", ylab = "Temperature (Degrees Celcius)")

# Decomposing the time series
Temp.dc <- decompose(Temp.ts)

# Plotting the decomposed time series
plot(Temp.dc)
# Error is additive since variance is constant along the time series.
# There is no trend.
# Seasonality is additive as well.

# Exploratory Data Analysis
# Test for normality
hist(Temp.ts, main = "Histogram of Temperature readings", xlab="Temperature")
qqnorm(Temp.ts, main = "Normal Q-Q plot of Temperature readings")
# From the above plots, the data seems to be normally distributed.
# Stationarity requires identical distribution.

# Box plot 

# Heatmap

# Partitioning the data
train.ts <- window(Temp.ts, start = c(1981, 1) ,end = c(1988, 12),freq=12)
valid.ts <- window(Temp.ts, start = c(1989, 1), end = c(1990, 12),freq=12)


length(valid.ts)

# Additive model with linear trend and seasonality 
ln <- tslm(train.ts ~ trend + season)
summary(ln)
valid.lnpred<-forecast(ln, h=24, level=0)         
accuracy(valid.lnpred,valid.ts)
##plot(ln)

abline(lm(Temperature ~ time(Temperature),data=Temp.data))

# Quadratic trend
qd <- tslm(train.ts ~ trend +I(trend^2)+ season)     
valid.qdpred<-forecast(qd, h=24, level=0)         
accuracy(valid.qdpred,valid.ts)

# ACF plot
acf(ln$residuals, main="ACF Plot for Additive model with linear trend and seasonality")

#** May have to group the daily data since the ets function can't handle data 
#*with frequency greater than 24**
# Holt-winter with ZZZ ACF
train.hwin1 <- ets(train.ts, model = "ZZZ")
# create ACF and PACF plots for the residuals
par(mfrow = c(2, 1))
acf(train.hwin1$residuals,  main="Holt-winter with ZZZ ACF")
pacf(train.hwin1$residuals, main="Holt-winter with ZZZ PACF")
# model validation on holdout
valid.hwin1pred <- forecast(train.hwin1, h = 24)
# accuracy() generates various types of errors between two time series
accuracy(valid.hwin1pred,valid.ts)

# Holt-winter with MAA ACF
train.hwin2 <- ets(train.ts, model = "AAA")
# create ACF and PACF plots for the residuals
par(mfrow = c(2, 1))
acf(train.hwin2$residuals,  main="Holt-winter with AAA ACF")
pacf(train.hwin2$residuals, main="Holt-winter with AAA PACF")
# model validation on holdout
valid.hwin2pred <- forecast(train.hwin2, h = 24)
# accuracy() generates various types of errors between two time series
accuracy(valid.hwin2pred,valid.ts)

#Differencing to deseasonalize
dcs<- diff(Temp.ts, lag = 12)
dcsp<-plot(decompose(dcs))

dct<- diff(Temp.ts, lag = 1)
dctp<-plot(decompose(dct))

dcst<-diff(diff(Temp.ts, lag = 1), lag = 12)
dcstp <- plot(decompose(dcst))

acf(dcs,main="ACF of deseasonalized data")
pacf(dcs,main="PACF of deseasonalized data")

#double differencing to estimate parameters
acf(diff(diff(Temp.ts, lag = 1), lag = 12), main='ACF of differenced data') #Estimates for q=0,1,2; Q= 1,2
pacf(diff(diff(Temp.ts, lag = 1), lag = 12), main='PACF of differenced data') #Estimates for p=0,1,2;P=1

# Auto-arima
train.autoarima <- auto.arima(train.ts)
valid.autoarimapred<-forecast(train.autoarima, h=24, level=0)  
accuracy(valid.autoarimapred,valid.ts)

# Arima(0,1,2)(2,1,1)
model1<-arima(train.ts, order=c(0,1,2), seasonal=c(2,1,1))
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals)))
valid.arimapred1<-forecast(model1, h=24, level=0)         
# accuracy() generates various types of errors between two time series
ACC<-as.data.frame(accuracy(valid.arimapred1,valid.ts))
train.rmse<-ACC[2,2]
test.rmse<-ACC[1,2]
model1.test
ACC

# Arima(1,1,2)(2,1,1)
model2<-arima(train.ts, order=c(1,1,2), seasonal=c(2,1,1))
model2.test<-Box.test(model2$residuals, lag = log(length(model1$residuals)))
valid.arimapred2<-forecast(model2, h=24, level=0)         
# accuracy() generates various types of errors between two time series
ACC<-as.data.frame(accuracy(valid.arimapred2,valid.ts))
train.rmse<-ACC[2,2]
test.rmse<-ACC[1,2]
model2.test
ACC

# Double Exponential Smoothing
des <- HoltWinters(train.ts,gamma = FALSE)
des$fitted
des.pred <-forecast(train.ts, h = 24, level = 95)
accuracy(des.pred,valid.ts)



## final results#######
## forecasting for next 1yr and next 5yrs
model2 %>% forecast(h=12) %>% plot()

model2 %>% forecast(h=60) %>% plot()




