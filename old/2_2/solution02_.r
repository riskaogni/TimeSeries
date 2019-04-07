#Лекция 6
options(warn=-1)

library(TSA)
library(lmtest)

X_data <- read.csv('./var02_Task02.csv')
X_data$Date <- as.Date(X_data$Date, "%d.%m.%Y")

head(X_data)

summary(X_data)

options(repr.plot.width=8, repr.plot.height=4)
plot(X_data$Date, X_data$Fact, type = 'l', col = 'blue', main = 'Initial Claims, USA', xlab = 'Time', ylab = 'Value')

#Легко видеть непостоянство среднего, из чего делаем вывод о нестационарности временного ряда
X = diff(X_data$Fact)
plot(X, type='l', col='blue', xlab='Time')

#Автоковариационная функция
acf(X, lwd=5, col='blue')
#Автокорреляцонная функция
pacf(X, lwd=5, col='blue')
#Exteded ACF
eacf(X)
#EACF не дает еще предпочтительных моделей

#Сравним модели с помощью теста Льюинг-Бокса (Урок 6)
#Здесь по сути проверяется гипотеза случайности- независимости и одинаковой распределенности, 
#но не в исходных данных, а именно в остатках после удаления модели ARMA, чем и объясняется его полезность

#Условный метод наименьших квадратов
model1 = arima(X, order=c(0, 0, 1), method='CSS')
model1$coef
#model1$sigma2

coeftest(model1)

Box.test(model1$residuals, lag = 6, type = "Ljung-Box", fitdf = 1)

options(repr.plot.width=4, repr.plot.height=4)
qqnorm(model1$residuals)
qqline(model1$residuals)
#Остатки явно не нормально распределены
#abline(0, 1)

#Проверим остатки на нормальность (еще раз убедились)
shapiro.test(model1$residuals)

#Построим acf остатков
options(repr.plot.width=8, repr.plot.height=4)
acf(model1$residuals, lwd = 5, col = "blue")
#Остатки НЕ представляют собой белый шум, следовательно делаем вывод о неадекватности модели.

#Рассмотрим другую модель
model2 = arima(X, order=c(0, 0, 2), method='CSS')
model2$sigma

coeftest(model2)

Box.test(model2$residuals, lag = 6, type = "Ljung-Box", fitdf = 1)

options(repr.plot.width=4, repr.plot.height=4)
qqnorm(model2$residuals)
qqline(model2$residuals)
#abline(0, 1)

shapiro.test(model2$residuals)

options(repr.plot.width=8, repr.plot.height=4)
acf(model2$residuals, lwd = 5, col = "blue")
#Остатки НЕ представляют собой белый шум, следовательно делаем вывод о неадекватности модели.

#Рассмотрим еще одну модель
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
#Остатки НЕ представляют собой белый шум, следовательно делаем вывод о неадекватности модели.


#Теперь сравним модели по тесту Акаике

#Оценка методом максимального правдоподобия
model1 = arima(X, order=c(0, 0, 1), method='ML')
model1$aic

model2 = arima(X, order=c(0, 0, 2), method='ML')
model2$aic

model3 = arima(X, order=c(8, 0, 0), method='ML')
model3$aic
#Откуда делаем вывод, что модель MA(2) наиболее предпочтительна.


#Сделаем предсказание по модели MA(2) на 5 интервалов времени вперед, 
#а так же построим для него доверительный интервал уровня 0.95.
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
    c(X_data$Fact, rep(NA, n_forecast)),
    c(rep(NA, length(X_data$Fact) - 1), diffinv(forecast$pred) + X_data$Fact[length(X_data$Fact)]),
    c(rep(NA, length(X_data$Fact)), forecast$pred[1] + u_conf_border + X_data$Fact[length(X_data$Fact)]),
    c(rep(NA, length(X_data$Fact)), forecast$pred[1] + l_conf_border + X_data$Fact[length(X_data$Fact)])
)
#Доверительный интервал
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


