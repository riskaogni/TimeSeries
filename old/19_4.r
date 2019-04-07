library(rJava)
library(xlsx)
library(tseries)
library(MASS)
library(cubature)
library(doParallel)
library(parallel)
library(foreach)
library(stats)
library(urca)
library(egcm)
library(mAr)

dat <- read.csv("C:/Users/Sony/Desktop/close_november.csv")

matplot(dat[,1],lwd=2,main="RTS November",type="l",col="blue")
matplot(dat[,2],lwd=2,main="Brent November",type="l",col="blue")
matplot(dat[,3],lwd=2,main="Si November",type="l",col="blue")
matplot(dat[,4],lwd=2,main="MIX November",type="l",col="blue")


vecm <- ca.jo(dat, ecdet = "none",type="eigen",K=2,spec="longrun")
summary(vecm)
vecm@cval
vecm@teststat
outmat <-rbind(vecm@cval[,3],vecm@teststat)
legends <- c("critical values","statistics")
barplot(outmat, main="Statistics vs Critical Value for 5% test ",legend.text = legends,beside = TRUE, col=c("red","blue"), las=3)

a<- vecm@V[,1:4]
a

dat<- as.matrix(dat)

z1 <- dat %*% vecm@V[,1]
matplot(z1,type ="l",col = "blue",lwd = 2,main ='z1= y*a1')
acf(z1)

z2 <- dat%*%vecm@V[,2]
matplot(z2,type ="l",col = "blue",lwd = 2,main ='z2= y*a2')
acf(z2)

z3 <- dat%*%vecm@V[,3]
matplot(z3,type ="l",col = "blue",lwd = 2,main ='z3= y*a3')
acf(z3)

z4 <- dat%*%vecm@V[,4]
matplot(z4,type ="l",col = "blue",lwd = 2,main ='z4= y*a4')
acf(z4)
