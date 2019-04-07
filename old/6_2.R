data = read.csv("./Dropbox/4 курс/прак/denislina/prak.csv", sep = ',')
head(data)
# y = data.frame(data$Дата, data$Факт)
y = data$Факт
n = length(y)
len = length(y)
plot(y)
plot(y[1:(n-1)], y[2:n],type='p',col = 'blue')
matplot(y, pch = 21, type = 'l')
y1 = y[2:n] - y[1:(n - 1)]
plot(y1[1:(n-1)], y1[2:n],type='p',col = 'blue')
y2 = y[2:n]/y[1:(n - 1)] - 1
plot(y2[1:(n-1)], y2[2:n],type='p',col = 'blue')


plot(y[1:(n-1)], y[2:n],type='p',col = 'blue')

plot(y[3:n], y[1:(n-2)], type='p',col = 'blue')

#Импортировать макро экономическую статистику EIA weekly crude stocks, 
#USA недельные данные за последние 5 лет. 
#К полученным данным идентифицировать и оценить две различных, но обоснованно 
#возможных ARMA модели. Выбрать из них наиболее адекватную (выбор Объяснить). 
#Провести анализ остатков от удаления модели:тип вероятностного распределения,
#автокоррелированность остатков. Построить прогноз и доверительный интервал 
#для прогноза уровня 0.95 на 5 интервалов времени вперед.

#При явной нестационарности ряда (обосновать) предварительно провести переход
#к преобразованным данным. Использовать преобразование данных вида:
#  yt=xt−xt1 или rt=xt/xt1−1.

library(fArma)
library(TSA)

acf(y)
pacf(y)
eacf(y)

# проверить  mod20 mod40 mod14
mod14 = arima (y, order = c(1, 0, 4))
mod40 = arima(y, order = c(4, 0, 4))
mod20 <- arima(y,order = c(2,0,0))
##########################################
mod = mod14
mod$coef
mod$sigma2
plot(mod,n.ahead=12,type='b',xlab='Time',col="blue",lty=3,n1=length(y)-20,lwd=1)
abline(h=coef(mod)[names(coef(mod))=='intercept'])
resar <- mod$residuals
plot.ts(resar,col = "blue",lwd = 2,type = "l", main = "residuals")

library(lmtest)
coeftest(mod)
Box.test(resar, lag = 12, type = "Ljung-Box", fitdf = 2)

qqnorm(resar)
qqline(resar)

acf(resar,lwd = 5, col = "blue")
##########################################
mod = mod20
mod$coef
mod$sigma2
plot(mod,n.ahead=12,type='b',xlab='Time',col="blue",lty=3,n1=length(y)-20,lwd=1)
abline(h=coef(mod)[names(coef(mod))=='intercept'])
resar <- mod$residuals
plot.ts(resar,col = "blue",lwd = 2,type = "l", main = "residuals")

library(lmtest)
coeftest(mod)
Box.test(resar, lag = 12, type = "Ljung-Box", fitdf = 2)

qqnorm(resar)
qqline(resar)

acf(resar,lwd = 5, col = "blue")
##################################################

mod = mod40
mod$coef
mod$sigma2
plot(mod,n.ahead=12,type='b',xlab='Time',col="blue",lty=3,n1=length(y)-20,lwd=1)
abline(h=coef(mod)[names(coef(mod))=='intercept'])
resar <- mod$residuals
plot.ts(resar,col = "blue",lwd = 2,type = "l", main = "residuals")

library(lmtest)
coeftest(mod)
Box.test(resar, lag = 12, type = "Ljung-Box", fitdf = 2)

qqnorm(resar)
qqline(resar)

acf(resar,lwd = 5, col = "blue")
##########################################
mod = mod14
forec<-predict(mod,n.ahead = 10)
forecpl <- forec$pred+forec$se
forecmn <- forec$pred-forec$se
ff <-cbind(forec$pred,forecpl,forecmn)
matplot(ff,lty= 1,type = "l",lwd = 2,main = "Forecasts +- std value" )


interest_residuals = mod$residuals
qqnorm(interest_residuals)
qqline(interest_residuals)

acf(interest_residuals,lwd = 5, col = "blue")
Box.test(interest_residuals, lag = 28, type = "Ljung-Box", fitdf = 2)

library(lmtest)
coeftest(mod)

acf(interest_residuals) #остатки коррелированы :(
# --------------------------------

getVector <- function(vv,bInd,l)
{
  c <- vector()
  if (bInd > 1)
  {
    c <- rep(NA,bInd-1)
  }
  res <- c(c,vv)
  cc <- vector()
  cc <- rep(NA,l-length(res))
  res <- c(res,cc)
  return(res)
  
}  
promData = y
lzz <- length(promData)
nforec <- 5
ll <- lzz+nforec
forecPromProizv<-predict(mod,n.ahead = nforec)
promData1 <- promData
vec1 <- getVector(promData1,1,ll)
vec2 <- getVector(forecPromProizv$pred,lzz+1,ll)
vec3 <- getVector(promData,1,ll)
q<- qnorm(0.9,0,1)

dPl <- forecPromProizv$pred + forecPromProizv$se*q
dMn <- forecPromProizv$pred - forecPromProizv$se*q
vec4 <- getVector(dPl,lzz+1,ll)
vec5 <- getVector(dMn,lzz+1,ll)
res <- cbind(vec1,vec2,vec3,vec4,vec5)
res <- res[200:ll,]
matplot(res ,  type = c("b","b","b","b","b"),main = "Forecast",pch = 16,lty=1, lwd= c(2,3,2,1,1),ylab = "Values",xlab="Time",col = c("green","blue","magenta","black","black"))
abline(h=coef(ml_ar6)[names(coef(ml_ar6))=='intercept'])
legend("topright",c("Forecasts","Time series","Conf.Level"),bty="n",lwd = 2,col = c("blue","magenta","black"))


last8 <- promData1[(lzz+1):length(promData1)] 
sum((last8-forecPromProizv$pred )^2)

forecAR_2<-predict(ml_ar2,n.ahead = nforec)
sum((last8-forecAR_2$pred )^2)

forecAR_1_1<-predict(ml_ar1_1,n.ahead = nforec)
sum((last8-forecAR_1_1$pred )^2)