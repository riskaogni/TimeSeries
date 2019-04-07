library(rJava)
library(xlsx)
library(tseries)
library(MASS)
library(cubature)
library(doParallel)
library(parallel)
library(foreach)
library(stats)

data <- read.csv("C:/Users/Sony/Desktop/industrial_output.csv")
data[,8] <- rev(data[,8])
promData <- ts(data[,8],start= c(1995,3),frequency = 12)
plot.ts(promData,col = "blue",lwd = 2,type = "l")
acf(promData,lwd = 5, col = "blue")
pacf(promData,lwd = 5, col = "blue")
#AR(4) or AR(6)
library(TSA)
eacf(promData)
#add ARMA(1,1)

ar4 <-  arima(promData,order = c(1,0,0),method="CSS")
ar4$coef
resar4 <- ar4$residuals
plot.ts(resar4,col = "blue",lwd = 2,type = "l", main = "residuals")

library(lmtest)
coeftest(ar4)
Box.test(resar4, lag = 12, type = "Ljung-Box", fitdf = 2)

qqnorm(resar4)
qqline(resar4)

acf(resar4,lwd = 5, col = "blue")
###########################################################################################################################
ar6 <-  arima(promData,order = c(6,0,0),method="CSS")
ar6$coef
resar6 <- ar6$residuals
plot.ts(resar2,col = "blue",lwd = 2,type = "l", main = "residuals")

library(lmtest)
coeftest(ar6)
Box.test(resar2, lag = 12, type = "Ljung-Box", fitdf = 2)

qqnorm(resar6)
qqline(resar6)

acf(resar6,lwd = 5, col = "blue")
#######################################################################################################################
ar_1_1 <-  arima(promData,order = c(1,0,1),method="CSS")
ar_1_1$coef
coeftest(ar_1_1)
resar1_1 <- ar_1_1$residuals
plot.ts(resar1_1,col = "blue",lwd = 2,type = "l", main = "residuals")
Box.test(resar1_1, lag = 12, type = "Ljung-Box", fitdf = 2)
qqnorm(resar1_1)
qqline(resar1_1)
acf(resar1_1,lwd = 5, col = "blue")

ml_ar6 <-  arima(promData,order = c(6,0,0),method="ML")
ml_ar4 <-  arima(promData,order = c(4,0,0),method="ML")
ml_ar1_1 <-  arima(promData,order = c(1,0,1),method="ML")
ml_ar1_1$aic
ml_ar6$aic
ml_ar4$aic
##########################################################################################################################
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
lzz <- length(promData)
nforec <- 5
ll <- lzz+nforec
forecPromProizv<-predict(ml_ar1_1,n.ahead = nforec)
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