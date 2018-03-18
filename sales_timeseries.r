# Time series
setwd("C://Users/dell//Desktop//Techdata_assignment//Time_Series")
sa=read.csv("sales.csv")
sa = ts(sa, start=2002, frequency = 12)
plot(sa)
acf(sa)
pacf(sa)

plot(diff(sa))
fit = arima(sa,order = c(1,1,1))
summary(fit)
predict(fit,n.ahead=12)
fit = arima(sa,order = c(2,1,2))
plot(diff(sa))
fit1 = arima(sa,order = c(1,1,0))
fit1
fit2 = arima(sa,order = c(0,1,1))
fit2
fit3= arima(sa,order = c(0,1,2))
fit3
fit4= arima(sa,order = c(0,0,2))
fit4
# To include seasonality
arima(sa,seasonal = list(order = c(1,1,1),period=4))
arima(sa,seasonal = list(order = c(1,1,1),period=3))
arima(sa,seasonal = list(order = c(1,1,1),period=5))
arima(sa,order = c(1,1,1),seasonal=list(order=c(1,2,1), period=4))
# Try another combination
fit5=arima(sa,order = c(1,1,1),seasonal=list(order=c(1,2,1), period=5))

tsdiag(fit5)
predict(fit, n.ahead = 8)
install.packages("forecast")
install.packages("quadprog")
library(forecast)
Arima_fit <- auto.arima(sa, approximation = FALSE,trace=FALSE)
summary(Arima_fit)
predict(Arima_fit, n.head=8)