library("TSA")
library(urca)
library(egcm)
library(mAr)
library(tseries)
Data <- read.csv("C:/Users/Анастасия/Desktop/Прак/CLOSE.csv", sep = ";")
head(Data)
Data <- Data[,3:6]
head(Data)
Data<- as.matrix(Data)
matplot(Data[,1],lwd=2,main="GAZR",type="l",col="blue")
matplot(Data[,2],lwd=2,main="SI",type="l",col="blue")
matplot(Data[,3],lwd=2,main="BR",type="l",col="blue")
matplot(Data[,4],lwd=2,main="RTS",type="l",col="blue")

vecm <- ca.jo(Data, ecdet = "none",type="eigen",K=2,spec="longrun")
summary(vecm)
outmat <-rbind(vecm@cval[,2],vecm@teststat)
legends <- c("critical values","statistics")
barplot(outmat, main="Statistics vs Critical Value for 5% test ",legend.text = legends,beside = TRUE, col=c("red","blue"), las=2)
# гипотезы до r<=1 не отвергается
z1 <- Data%*%vecm@V[,1]
adf.test(z1, alternative="stationary") #стационарен
matplot(z1,type ="l",col = "blue",lwd = 2,main ='z1= y*a1')

z2 <- Data%*%vecm@V[,2]
adf.test(z2, alternative="stationary")
matplot(z2,type ="l",col = "blue",lwd = 2,main ='z2= y*a2')

z3 <- Data%*%vecm@V[,3]
adf.test(z3, alternative="stationary")
matplot(z3,type ="l",col = "blue",lwd = 2,main ='z3= y*a3')

z4 <- Data%*%vecm@V[,4]
adf.test(z4, alternative="stationary")
matplot(z4,type ="l",col = "blue",lwd = 2,main ='z4= y*a4')

