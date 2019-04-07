Data <- read.csv("C:/Users/user/Documents/R/tasks/t2.csv",header = TRUE)
head(Data)
Data$values <- ts(Data$values,start= c(1995,1),frequency = 12)
plot.ts(Data$values,col = "blue")
acf(Data$values,lwd = 5, col = "blue")
pacf(Data$values,lwd = 5, col = "blue")

library(lmtest)


## AR(3) модель
ar3 <-  arima(Data$values,order = c(3,0,0),method="CSS")
ar3$coef
ar3$sigma2
coeftest(ar3)
resar3 <- ar3$residuals
plot.ts(resar3,col = "blue",lwd = 2,type = "l", main = "residuals")
Box.test(resar3, lag = 12, type = "Ljung-Box", fitdf = 2)
qqnorm(resar3)
qqline(resar3)
acf(resar3,lwd = 5, col = "blue")



## AR(15) модель
ar15 <-  arima(Data$values,order = c(15,0,0),method="CSS")
ar15$coef
ar15$sigma2
coeftest(ar15)
resar15 <- ar15$residuals
plot.ts(resar15,col = "blue",lwd = 2,type = "l", main = "residuals")
Box.test(resar15, lag = 12, type = "Ljung-Box", fitdf = 2)
qqnorm(resar15)
qqline(resar15)
acf(resar15,lwd = 5, col = "blue")


library(TSA)
eacf(Data$values)

##смешанная модель

ar_1_1 <-  arima(Data$values,order = c(1,0,1),method="ML")
ar_1_1$coef
ar_1_1$sigma2
coeftest(ar_1_1)
resar1_1 <- ar_1_1$residuals
plot.ts(resar1_1,col = "blue",lwd = 2,type = "l", main = "residuals")
Box.test(resar1_1, lag = 12, type = "Ljung-Box", fitdf = 2)
qqnorm(resar1_1)
qqline(resar1_1)
acf(resar1_1,lwd = 5, col = "blue")



## сравнение моделей по тесту Акаике
ml_ar15 <-  arima(Data$values,order = c(15,0,0),method="ML")
ml_ar3 <-  arima(Data$values,order = c(3,0,0),method="ML")
ml_ar1_1 <-  arima(Data$values,order = c(1,0,1),method="ML")
ml_ar1_1$aic
ml_ar3$aic
ml_ar15$aic


##определение типа распределения остатков для AR(3)
hist(resar3,xlab='Standardized Residuals', col = "blue")

## Лаплас
library(LaplacesDemon)
med <- median(resar3)
n <- length(resar3)
sum <- 0
for (i in 1:n)
  sum <- sum + abs(resar3[i] - med)
mu <- n / sum  
ks.test(resar3, "plaplace", 0, mu)

## t-Стьюдента
library(fGarch)
mu <- var(resar3)
ks.test(resar3, "psstd", 0, mu)

##некоррелируемость
acf(resar3)

##нестационарность ряда
y_t_1 <- Data$values[2:n]
y_t_1[n] = 0
y_t <-Data$values [1:n]
plot(y_t_1, y_t)

##прогнозирование
z = diff(Data$values)
ar_3 <-  arima(z,order = c(3,0,0),method="CSS")

forecast <- predict(ar_3, n.ahead = 5)
y <- diffinv(forecast$pred)
se <- diffinv(forecast$se)
q <- qnorm(0.95,0,1)
dPl <- y + se*q
dMn <- y - se*q
res = cbind(y,dPl,dMn)
matplot(res ,  type = c("b","b","b"),main = "Forecast",pch = 16,lty=1, lwd= c(2,1,1),ylab = "Values",xlab="Time",col = c("magenta","black","black"))

