library("TSA")
library(lmtest)
library(tseries)
Data <- read.csv("C:/Users/Анастасия/Desktop/Прак/Retail Sales.csv")
head(Data)
Data <- ts(Data,start= c(1999,1),frequency = 1)
plot.ts(Data, col = "blue",lwd = 2,type = "l")
adf.test(Data, alternative="stationary") #стационарен
acf(Data,lwd = 5, col = "blue")
pacf(Data,lwd = 5, col = "blue")
eacf(Data) # ma1,

arma <- arima(Data, c(0, 0, 1), method = "ML")
arma$aic
coeftest(arma)
arma2 <- arima(Data, c(0, 0, 6), method = "ML")
arma2$aic
coeftest(arma2)


r<- arma$residuals
acf(r)


newreg<- (r - mean(r))/sd(r)

hist(newreg,col = "blue", prob=TRUE)

ks.test(newreg, "plaplace")
ks.test(newreg, "pnorm")

library("fGarch")
f<- stdFit(r)
fs<- sstdFit(r)

sf<- f$par
sfs <- fs$estimate

ks.test(r, "pstd", sf["mean"], sf["sd"], sf ["nu"])
ks.test(r, "psstd", sfs["mean"], sfs["sd"], sfs["nu"], sfs["xi"])


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
forecData<-predict(arma,n.ahead = nforec)

vec2 <- getVector(forecData$pred,lzz+1,ll)
vec3 <- getVector(Data,1,ll)


dPl <- forecData$pred + forecData$se*3
dMn <- forecData$pred - forecData$se*3
vec4 <- getVector(dPl,lzz+1,ll)
vec5 <- getVector(dMn,lzz+1,ll)
res <- cbind(vec2,vec3,vec4,vec5)

matplot(res ,  type = c("b","b","b","b"),main = "Forecast",pch = 16,lty=1, lwd= c(3,2,1,1),ylab = "Values",xlab="Time",col = c("blue","magenta","black","black"))
abline(h=coef(arma)[names(coef(arma))=='intercept'])
legend("topright",c("Forecasts","Time series","Conf.Level"),bty="n",lwd = 2,col = c("blue","magenta","black"))

