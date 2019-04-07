Data1 <- read.csv("C:/Users/Nikolay/Desktop/R/var7/var_ 7Task2.csv",header = TRUE)
head(Data1)

Data1 <- ts(Data1$Close,start= 1,frequency = 1)
plot.ts(Data1, col = "blue",lwd = 2,type = "l")

library("TSA")
library(lmtest)
library(tseries)
#adf.test(Data1, alternative="stationary") #стационарен

#Data <- ts(Data$Stat,start= 1,frequency = 1)
#plot.ts(Data, col = "blue",lwd = 2,type = "l")

adf.test(Data1, alternative="explosive")
Data = diff(Data1)
plot.ts(Data, col = "blue",lwd = 2,type = "l")
acf(Data,lwd = 5, col = "blue")
pacf(Data,lwd = 5, col = "blue")

#eacf(Data)

#Оценка методом максимального правдоподобия
ar50 <- arima(Data, c(5, 0, 8), method = "ML")
ar50$coef
ar50$sigma2
coeftest(ar1)
ar50$aic
#Метод моментов (только для AR(p) моделей) с автоматическим выбором порядка модели по критерию Акаике
ar5_0 <- ar(Data,method="yule-walker")
ar5_0 <- ar(Data,method="yw")
ar5_0$order
#оцениваем остатки
resar50 <- ar50$residuals
hist(resar50,xlab='Standardized Residuals', col = "blue")
plot.ts(resar50,col = "blue",lwd = 2,type = "l", main = "residuals")
qqnorm(resar50)
qqline(resar50)

library(LaplacesDemon)
d=sd(resar50)
a= 2/ sqrt(d)
ks.test(resar50, "plaplace",0,a)

#Тест Льюнг-Бокса
Box.test(resar50, lag = 19, type = "Ljung-Box", fitdf = 1)
acf(resar1,lwd = 5, col = "blue")





#Прогноз
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
lzz <- length(Data)
nforec <- 5
ll <- lzz+nforec
forecPromProizv<-predict(ar50,n.ahead = nforec)

vec2 <- getVector(forecPromProizv$pred,lzz+1,ll)
vec3 <- getVector(Data,1,ll)
q<- qnorm(0.95,0,1)

dPl <- forecPromProizv$pred + forecPromProizv$se*q
dMn <- forecPromProizv$pred - forecPromProizv$se*q
vec4 <- getVector(dPl,lzz+1,ll)
vec5 <- getVector(dMn,lzz+1,ll)
res <- cbind(vec2,vec3,vec4,vec5)
res <- res[200:ll,]
matplot(res,  type = c("b","b","b","b"),main = "Forecast",pch = 16,lty=1, lwd= c(3,2,1,1),ylab = "Values",
        xlab="Time",col = c("blue","magenta","black","black"))
abline(h=coef(ar1)[names(coef(ar1))=='intercept'])
legend("topleft",c("Forecasts","Time series","Conf.Level"),bty="n",lwd = 2,col = c("blue","magenta","black"))

