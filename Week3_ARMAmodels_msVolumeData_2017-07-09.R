### ~*~*~*~*~*~*           Rebecca Vislay Wade          ~*~*~*~*~*~* ###
### ~*~*~*~*~*~* PREDICT 413: Time Series & Forecasting ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*   Summer 2017 | Section 57 | NU MSPA   ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~ July 9, 2017 *~*~*~*~*~*~*~*~*~*~*~*~*~* ###

### This program preliminarily creates, plots, and compares time series
### models using simple exponential smoothing (ses) and autoregressive
### moving average (ARMA) models. Four different ARMA models are produced:
### (0,0), (0,1), (1,0), and (1,1).

# Load some libraries.
library(forecast)
library(tseries)

# Read in from .csv
daily = read.csv("~/NU_MSPA/R_work/PREDICT_413/data/dailyVolumeCleaned.csv", header = TRUE)

# Convert dates:
daily$date = as.Date(daily$date, origin = "1899-12-30")

# Convert to time series (ts) object and create 6 month chunk.
tsDaily = ts(daily$mss)
tsDaily6mos = ts(tsDaily[130:253])        # Jul-Dec 2016

# SES model with next 10 predicted points
sesModel = ses(tsDaily6mos, alpha = 0.1, h=10)

# ADF test for stationarity:
adf6mos = adf.test(tsDaily6mos, alternative = "s")
adf6mos

# tsdisplay() to look at ACF and PACF plots
tsdisplay(tsDaily6mos, main = "Daily Manuscript Volume, Jul - Dec 2016")

# ARMA models using the arima() function
arma01 = arima(tsDaily6mos, order = c(0,0,0))
arma02 = arima(tsDaily6mos, order = c(1,0,0))
arma03 = arima(tsDaily6mos, order = c(0,0,1))
arma04 = arima(tsDaily6mos, order = c(1,0,1))
# h = 10 forecast predictions + fits
arma01pred = forecast(arma01, h = 10)
arma02pred = forecast(arma02, h = 10)
arma03pred = forecast(arma03, h = 10)
arma04pred = forecast(arma04, h = 10)
# Box-Ljung tests for residuals
sesBox = Box.test(sesModel$residuals, lag=20, type="Ljung-Box")
arma01box = Box.test(arma01pred$residuals, lag=20, type="Ljung-Box")
arma02box = Box.test(arma02pred$residuals, lag=20, type="Ljung-Box")
arma03box = Box.test(arma03pred$residuals, lag=20, type="Ljung-Box")
arma04box = Box.test(arma04pred$residuals, lag=20, type="Ljung-Box")

# Plot fits:
ts.plot(tsDaily6mos, sesModel$fitted, arma01pred$fitted, arma02pred$fitted, 
        arma03pred$fitted, arma04pred$fitted,
        gpars = list(col = c("black", "green4", "red4", "mediumorchid3", 
                             "deepskyblue3", "darkorange3"), 
                     lwd = 3.0, cex.axis = 1.2, xlim = c(0,135)),
        xlab = "")
title(xlab = "Days", ylab = "Manuscripts Received", 
      main = "SES versus ARMA Fits", 
      cex.lab = 1.3, cex.main = 1.5)
grid(nx = NULL, ny = NULL, col = "gray45", lty = "dotted",
     lwd = 1.0)
legend("bottomleft", inset=c(0,0), xpd = TRUE, legend = c("Jul-Dec 2016", "SES 0.1", 
                                                          "ARMA(0,0)", "ARMA(1,0)", 
                                                          "ARMA(0,1)", "ARMA(1,1)"), 
       col = c("black", "green4", "red4", "mediumorchid3", "deepskyblue3", "darkorange3"), 
       lty = 1,lwd = 3.0, title = "Legend", cex = 1.0)
lines(sesModel$mean, col="green4", type = "o", pch=19, cex = 1.2)
lines(arma01pred$mean, col="red4", type = "o", pch=19, cex = 1.2)
lines(arma02pred$mean, col="mediumorchid3", type = "o", pch=19, cex = 1.2)
lines(arma03pred$mean, col="deepskyblue3", type = "o", pch=19, cex = 1.2)
lines(arma04pred$mean, col="darkorange3", type = "o", pch=19, cex = 1.2)

# close up of days 90-140
ts.plot(tsDaily6mos, sesModel$fitted, arma01pred$fitted, arma02pred$fitted, 
        arma03pred$fitted, arma04pred$fitted,
        gpars = list(col = c("black", "green4", "red4", "mediumorchid3", 
                             "deepskyblue3", "darkorange3"), 
                     lwd = 3.0, cex.axis = 1.2, xlim = c(110,135), ylim = c(100,260)),
        xlab = "")
grid(nx = NULL, ny = NULL, col = "gray45", lty = "dotted",
     lwd = 1.0)
lines(sesModel$mean, col="green4", type = "o", pch=19, cex = 1.2)
lines(arma01pred$mean, col="red4", type = "o", pch=19, cex = 1.2)
lines(arma02pred$mean, col="mediumorchid3", type = "o", pch=19, cex = 1.2)
lines(arma03pred$mean, col="deepskyblue3", type = "o", pch=19, cex = 1.2)
lines(arma04pred$mean, col="darkorange3", type = "o", pch=19, cex = 1.2)


AICs = c(sesModel$model$aic, arma01pred$model$aic,
         arma02pred$model$aic, arma03pred$model$aic,
         arma04pred$model$aic)

autoAR = auto.arima(tsDaily6mos, ic = "aic")
autoAR
