#installing libraries 
install.packages('forecast')
library(forecast)
install.packages(stats)
library(stats)
#reading data
d = read.table("C:\\Users\\Lenovo\\Desktop\\rita_assignment\\oil.price.dat.txt",sep="\t",header = TRUE)
#forming time series
full_series <- ts(d, frequency=12, start=c(1986,1))
#series 1 to predict 12 months
op_timeseries_a <- ts(d, frequency=12, start=c(1986,1), end = c(2005,1))
#series 2 to predict 6 months
op_timeseries_b <- ts(d, frequency=12, start=c(1986,1), end = c(2005,7))

#Exploratory data analysis
plot.ts(op_timeseries_a)
plot.ts(op_timeseries_b)

#Decomposing time series
op_timeseries_a.stl = stl(op_timeseries_a, s.window="periodic")
plot(op_timeseries_a.stl)
#Confirming whether times series has Tren and seasonality
x <- op_timeseries_a
apply(op_timeseries_a.stl$time.series, 2, var) / var(x)
#   seasonal       trend   remainder 
#  0.007531872 0.870525910 0.079840511
y <- op_timeseries_b
apply(op_timeseries_b.stl$time.series, 2, var) / var(y)
#   seasonal       trend   remainder 
#  0.004283982 0.916342649 0.056080131

op_timeseries_b.stl = stl(op_timeseries_b, s.window="periodic")
plot(op_timeseries_b.stl)

#Model building

#Simple exponential smoothing-Holts model because data has trend but no seasonality

exp_sm_a <- HoltWinters(op_timeseries_a, gamma=FALSE)
exp_sm_b <- HoltWinters(op_timeseries_b, gamma=FALSE)
exp_sm
exp_sm_forecast_a <- forecast(exp_sm_a, h=12)
exp_sm_forecast_b <- forecast(exp_sm_b, h=6)
accuracy(exp_sm_forecast_a)
accuracy(exp_sm_forecast_b)

exp_sm_forecast_a
exp_sm_forecast_b
plot(exp_sm_fit)

#Arima model

#Checking Stationary test #Dicker Fuller test
install.packages('tseries')
install.packages('fUnitRoots')
install.packages('forecast')
library(tseries)
library(fUnitRoots)
library(forecast)

#Test 1
adf.test((op_timeseries_a), alternative = "stationary", k=0)
#p-value = 0.5367
#Test 2
adfTest(op_timeseries_a, lags = 0, type = "c")
#P VALUE:0.7897

#both tests have p value greater than 0.05. So we cannot reject non stationarity with confidence

#convert to stationarity
diff1 <- diff(op_timeseries_a, differences=1)
adf.test((diff1), alternative = "stationary", k=0)
ar<-auto.arima(op_timeseries_a)
arima_f<- arima(op_timeseries_a, order=c(2,1,0)) 
fit<-(forecast(arima_f,h=12))
plot(fit)
accuracy(ar)
ar
#Auto arima
ar_b<-auto.arima(op_timeseries_b)
arima_f_b<- arima(op_timeseries_b, order=c(2,1,2)) 
fit<-(forecast(arima_f_b,h=6))
plot(fit)
accuracy(arima_f_b)
ar