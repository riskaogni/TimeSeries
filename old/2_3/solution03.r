
options(warn=-1)

X <- read.csv('./data.csv')
head(X)

X <- X[,-3]
summary(X)

X_std <- X
# X_std$BR   <- scale(X_std$BR, center = TRUE, scale = TRUE)
X_std$GAZR <- scale(X_std$GAZR, center = TRUE, scale = TRUE)
X_std$MIX  <- scale(X_std$MIX, center = TRUE, scale = TRUE)
X_std$SBRF <- scale(X_std$SBRF, center = TRUE, scale = TRUE)

options(repr.plot.width=8, repr.plot.height=4)

matplot(
    X_std,
    type = "l", lty = 1, col = c("green", "blue", "magenta"),
    main = "Standardizied prices"
)

legend(
    "topleft",
    c("GAZR", "MIX", "SBRF"),
    bty = "n", lwd = 2,
    col = c("green", "blue", "magenta"),
    cex = 0.8,
    y.intersp = 2
)

X_rates <- X[-1, ] / X[-dim(X)[1], ] - 1
X_rates <- X_rates[, -c(1, 2)]

head(X_rates)

options(repr.plot.width=8, repr.plot.height=4)
matplot(X_rates, type = "l", lty = 1, main = "Rates", col = c("green", "blue", "magenta"))

legend(
    "topleft",
    c("GAZR", "MIX", "SBRF"),
    bty = "n", lwd = 2,
    col = c("green", "blue", "magenta"),
    cex = 0.8,
    y.intersp = 2
)

library(vars)

nfocrit <- VARselect(X_rates, lag.max = 5, type="const")
nfocrit

varsimest <- VAR(X_rates, p = 1, season = NULL, exogen = NULL)
summary(varsimest)

n_forecast = 2
forecast <- predict(varsimest, n.ahead = n_forecast, ci = 0.95)

options(repr.plot.width=4, repr.plot.height=3)
matplot(
    forecast$fcst$GAZR,
    type = "b", pch = 21, lty = 1, lwd = 3,
    col = c("blue", "black", "black", "red"),
    main = "Forecast of GAZR"
)

matplot(
    forecast$fcst$MIX,
    type = "b", pch = 21, lty = 1, lwd = 3,
    col = c("blue", "black", "black", "red"),
    main = "Forecast of MIX"
)

matplot(
    forecast$fcst$SBRF,
    type = "b", pch = 21, lty = 1, lwd = 3,
    col = c("blue", "black", "black", "red"),
    main = "Forecast of SBRF"
)

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

options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(X_rates$GAZR, forecast$fcst$GAZR, 'GAZR')

options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(X_rates$MIX, forecast$fcst$MIX, 'MIX')

options(repr.plot.width=8, repr.plot.height=4)
draw_prediction_rates(X_rates$SBRF, forecast$fcst$SBRF, 'SBRF')

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

draw_prediction(X$GAZR, forecast$fcst$GAZR, 'GAZR')

draw_prediction(X$MIX, forecast$fcst$MIX, 'MIX')

draw_prediction(X$SBRF, forecast$fcst$SBRF, 'SBRF')


