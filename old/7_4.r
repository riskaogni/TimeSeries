data <- read.csv('C:/Users/Nikolay/Desktop/R/var7/var_ 7Task4.csv')
head(data)

data <- data[,-1]
data <- data[,-1]
head(data)
summary(data)

data_std <- data
data_std$BR   <- scale(data_std$BR, center = TRUE, scale = TRUE)
data_std$RTS <- scale(data_std$RTS, center = TRUE, scale = TRUE)
data_std$MIX  <- scale(data_std$MIX, center = TRUE, scale = TRUE)
data_std$SBRF <- scale(data_std$SBRF, center = TRUE, scale = TRUE)

#options(repr.plot.width=8, repr.plot.height=4)

matplot(
  data_std,
  type = "l", lty = 1, col = c("green", "blue", "magenta", "black"),
  main = "Standardizied prices"
)

legend(
  "topleft",
  c("BR","RTS", "MIX", "SBRF"),
  bty = "n", lwd = 2,
  col = c("green", "blue", "magenta", "black"),
  cex = 0.8,
  y.intersp = 2
)

data_rates <- data[-1,] / data[-dim(data)[1], ] - 1
head(data_rates)


#options(repr.plot.width=8, repr.plot.height=4)
matplot(data_rates, type = "l", lty = 1, main = "Rates", col = c("green", "blue", "magenta", "black"))

legend(
  "topleft",
  c("BR","RTS", "MIX", "SBRF"),
  bty = "n", lwd = 2,
  col = c("green", "blue", "magenta"),
  cex = 0.8,
  y.intersp = 2
)

library(vars)

nfocrit <- VARselect(data_rates, lag.max = 5, type="const")
nfocrit

varsimest <- VAR(data_rates, p = 1, season = NULL, exogen = NULL)
summary(varsimest)


n_forecast = 2
forecast <- predict(varsimest, n.ahead = n_forecast, ci = 0.95)



draw_prediction_rates <- function(rates, forecast, name) {
  result <- cbind(
    c(rates, rep(NA, n_forecast)),
    c(rep(NA, length(rates)), forecast[,1]),
    c(rep(NA, length(rates)), forecast[,2]),
    c(rep(NA, length(rates)), forecast[,3])
  )
  
  matplot(
    result[200:dim(result)[1],], 
    type = rep('b', 4),
    main = paste0(name, ". Forecast. Rates"), 
    pch = 16, lty=1, lwd= c(2,3,1,1),
    ylab = "Values",
    xlab = "Time",
    col = c("magenta","blue", "black","black")
  )
  
  legend(
    "topleft",
    c("Time series", "Forecasts", "Conf.Level"),
    bty = "n", lwd = 2,
    col = c("magenta", "blue", "black"),
    cex = 0.8,
    y.intersp = 2
  )
}

#options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(data_rates$BR, forecast$fcst$BR, 'BR')

#options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(data_rates$RTS, forecast$fcst$RTS, 'RTS')

#options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(data_rates$MIX, forecast$fcst$MIX, 'MIX')

#options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(data_rates$SBRF, forecast$fcst$SBRF, 'SBRF')

draw_prediction <- function(ts, forecast, name) {
  v1 <- c(ts[length(ts)])
  for (i in forecast[,1]) {
    v1 <- c(v1, v1[length(v1)] * (i + 1))
  }
  
  v2 <- c(ts[length(ts)])
  for (i in forecast[,2]) {
    v2 <- c(v2, v2[length(v2)] * (i + 1))
  }
  
  v3 <- c(ts[length(ts)])
  for (i in forecast[,3]) {
    v3 <- c(v3, v3[length(v3)] * (i + 1))
  }
  
  result <- cbind(
    c(ts, rep(NA, n_forecast)),
    c(rep(NA, length(ts) - 1), v1),
    c(rep(NA, length(ts) - 1), v2),
    c(rep(NA, length(ts) - 1), v3)
  )
  
  matplot(
    result[200:dim(result)[1],], 
    type = rep('b', 4),
    main = paste0(name, ". Forecast. Rates"), 
    pch = 16, lty=1, lwd= c(2,3,1,1),
    ylab = "Values",
    xlab = "Time",
    col = c("magenta","blue", "black","black")
  )
  
  legend(
    "topleft",
    c("Time series", "Forecasts", "Conf.Level"),
    bty = "n", lwd = 2,
    col = c("magenta", "blue", "black"),
    cex = 0.8,
    y.intersp = 2
  )
}

draw_prediction(data$BR, forecast$fcst$BR, 'BR')

draw_prediction(data$RTS, forecast$fcst$RTS, 'RTS')

draw_prediction(data$MIX, forecast$fcst$MIX, 'MIX')

draw_prediction(data$SBRF, forecast$fcst$SBRF, 'SBRF')

