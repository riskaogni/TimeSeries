
options(warn=-1)

library(TSA)
library(lmtest)

X_data <- read.csv('./var02_Task02.csv')
X_data$Дата <- as.Date(X_data$Дата, "%d.%m.%Y")

head(X_data)

summary(X_data)

options(repr.plot.width=8, repr.plot.height=4)
plot(X_data$Дата, X_data$Факт, type = 'l', col = 'blue', main = 'Initial Claims, USA', xlab = 'Time', ylab = 'Value')

# X = tail(X_data$Факт, -1) - head(X_data$Факт, -1)

X = diff(X_data$Факт)
plot(X, type='l', col='blue', xlab='Time')

acf(X, lwd=5, col='blue')

pacf(X, lwd=5, col='blue')

eacf(X)

model1 = arima(X, order=c(0, 0, 1), method='CSS')
model1$sigma2

coeftest(model1)

Box.test(model1$residuals, lag = 6, type = "Ljung-Box", fitdf = 1)

options(repr.plot.width=4, repr.plot.height=4)
qqnorm(model1$residuals)
abline(0, 1)

shapiro.test(model1$residuals)

options(repr.plot.width=8, repr.plot.height=4)
acf(model1$residuals, lwd = 5, col = "blue")

model2 = arima(X, order=c(0, 0, 2), method='CSS')
model2$sigma

coeftest(model2)

Box.test(model2$residuals, lag = 6, type = "Ljung-Box", fitdf = 1)

options(repr.plot.width=4, repr.plot.height=4)
qqnorm(model2$residuals)
abline(0, 1)

shapiro.test(model2$residuals)

options(repr.plot.width=8, repr.plot.height=4)
acf(model2$residuals, lwd = 5, col = "blue")

model3 = arima(X, order=c(8, 0, 0), method='CSS')
model3$sigma

coeftest(model3)

Box.test(model2$residuals, lag = 12, type = "Ljung-Box", fitdf = 1)

options(repr.plot.width=4, repr.plot.height=4)
qqnorm(model2$residuals)
abline(0, 1)

shapiro.test(model3$residuals)

options(repr.plot.width=8, repr.plot.height=4)
acf(model3$residuals, lwd = 5, col = "blue")

model1 = arima(X, order=c(0, 0, 1), method='ML')
model1$aic

model2 = arima(X, order=c(0, 0, 2), method='ML')
model2$aic

model3 = arima(X, order=c(8, 0, 0), method='ML')
model3$aic

n_forecast <- 5

forecast <- predict(model2, n.ahead = n_forecast)

q <- qnorm(0.975, 0, 1)
u_conf_border <- forecast$pred + q * forecast$se
l_conf_border <- forecast$pred - q * forecast$se

result <- cbind(
    c(X, rep(NA, n_forecast)),
    c(rep(NA, length(X)), forecast$pred),
    c(rep(NA, length(X)), u_conf_border),
    c(rep(NA, length(X)), l_conf_border)
)

options(repr.plot.width=8, repr.plot.height=4)

matplot(
    result[200:dim(result)[1],], 
    type = rep('b', 4),
    main = "Forecast. Differences", 
    pch = 16, lty=1, lwd= c(2,3,1,1),
    ylab = "Values",
    xlab = "Time",
    col = c("magenta","blue", "black","black")
)

abline(h=coef(model2)[names(coef(model2))=='intercept'])

legend(
    "topleft",
    c("Time series", "Forecasts", "Conf.Level"),
    bty = "n", lwd = 2,
    col = c("magenta","blue","black"),
    cex = 0.8,
    y.intersp = 2
)

result <- cbind(
    c(X_data$Факт, rep(NA, n_forecast)),
    c(rep(NA, length(X_data$Факт) - 1), diffinv(forecast$pred) + X_data$Факт[length(X_data$Факт)]),
    c(rep(NA, length(X_data$Факт)), forecast$pred[1] + u_conf_border + X_data$Факт[length(X_data$Факт)]),
    c(rep(NA, length(X_data$Факт)), forecast$pred[1] + l_conf_border + X_data$Факт[length(X_data$Факт)])
)

matplot(
    result[200:dim(result)[1],],
    type = rep('b', 4),
    main = "Forecast. Time Series", 
    pch = 16, lty=1, lwd= c(2,3,1,1),
    ylab = "Values",
    xlab="Time",
    col = c("magenta","blue", "black","black")
)

legend(
    "bottomleft",
    c("Time series", "Forecasts", "Conf.Level"),
    bty = "n", lwd = 2,
    col = c("magenta","blue","black"),
    cex = 0.8,
    y.intersp = 2
)


