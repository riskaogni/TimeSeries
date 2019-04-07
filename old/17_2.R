library(fArma)
library(TSA)
library(lmtest)

setwd("/home/helensqr/prac/var17")
task2_data <- read.csv("./task2.csv")
task2_data <- ts(task2_data, start=c(2010, 1), frequency=12)
plot.ts(task2_data, col="blue", lwd=2, type="l")
acf(task2_data, lwd=5, col="blue")
pacf(task2_data, lwd=5, col="blue")
eacf(task2_data)

ar_5 <- arima(task2_data, order=c(5, 0, 0), method="CSS")
coeftest(ar_5)
resar5 <- ar_5$residuals
Box.test(resar5, lag = 12, type = "Ljung-Box", fitdf = 2)

ma_7 <- arima(task2_data, order=c(0, 0, 7), method="CSS")
coeftest(ma_7)
resar7 <- ma_7$residuals
Box.test(resar7, lag = 12, type = "Ljung-Box", fitdf = 2)

ar_1_1 <- arima(task2_data, order=c(1, 1, 0), method="CSS")
coeftest(ar_1_1)
resar1_1 <- ar_1_1$residuals
Box.test(resar1_1, lag = 12, type = "Ljung-Box", fitdf = 2)

qqnorm(resar5, col="green")
qqline(resar5)

qqnorm(resar7, col="blue")
qqline(resar7)

qqnorm(resar1_1)
qqline(resar1_1, col="red")

ml_ar6 <-  arima(task2_data,order = c(6,0,0),method="ML")
ml_ma7 <-  arima(task2_data,order = c(0,0,7),method="ML")
ml_ar1_1 <-  arima(task2_data,order = c(1,0,1),method="ML")
ml_ar6$aic
ml_ma7$aic
ml_ar1_1$aic