### ~*~*~*~*~*~*           Rebecca Vislay Wade          ~*~*~*~*~*~* ###
### ~*~*~*~*~*~* PREDICT 413: Time Series & Forecasting ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*   Summer 2017 | Section 57 | NU MSPA   ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~ July 25, 2017 *~*~*~*~*~*~*~*~*~*~*~*~*~* ###

### This program compares the stationarity of daily measured data with the
### same data binned by week (with some earlier time points included). 
### ARIMA models are built using auto.arima() and arima() functions.

# Load some libraries.
library(tseries)
library(forecast)
library(xts)

# Daily manuscript volume data
daily = read.csv("~/NU_MSPA/R_work/PREDICT_413/data/dailyVolumeCleaned.csv", header = TRUE)

# Read in weekly manuscript volume data from .csv
weekly = read.csv("~/NU_MSPA/R_work/PREDICT_413/data/weeklyVolume.csv", header = TRUE)

# Convert dates (origin argument is specific for converting Windows-version
# of Excel dates since the data file was created in Excel):
daily$date = as.Date(daily$date, origin = "1899-12-30")
weekly$Start.Date = as.Date(weekly$Start.Date, origin = "1899-12-30")
weekly$End.Date = as.Date(weekly$End.Date, origin = "1899-12-30")

# Convert to time series object, ordered by week start date
tsDaily = xts(daily$mss, order.by=as.Date(daily$date), frequency = 1)
tsWeekly = xts(weekly$mss, order.by=as.Date(weekly$Start.Date), frequency = 7)

# ts displays to look at data, ACF, and PACF plots
tsdisplay(tsDaily, main = "Daily Manuscript Volume - Jan 2016 to Jun 2017")
tsdisplay(tsWeekly, main = "Weekly Manuscript Volume - Jan 2015 to Jun 2017")

# Stationarity tests for weekly data
adfWeekly = adf.test(tsWeekly, alternative = "stationary")
adfWeekly    # Returns a p-value < 0.01; reject null hypothesis of non-stationarity

kpssWeekly = kpss.test(tsWeekly, null = "Level")
kpssWeekly   # returns a p-value < 0.01; reject null hypothesis of stationarity

# Even though the ADF test indicates stationarity, the ACF from tsdisplay() 
# and the KPSS test suggest that differencing is needed. ndiffs() is used to 
# find the order of differencing 
# needed.
numDiffs = ndiffs(tsWeekly)
numDiffs

# ndiffs() suggests first order differencing...
tsWeeklyDiff = diff(tsWeekly, differences = 1)

# tsdisplay() to look at the differenced series
tsdisplay(tsWeeklyDiff, main = "First-Order Differenced Weekly Manuscript Volume - Jan 2015 to Jun 2017")

# omit NA (value for the first week) in differenced data
tsWeeklyDiff = na.omit(tsWeeklyDiff)

# mean subtracted tsWeekly
tsWeeklyAdj = xts(tsWeekly - mean(tsWeekly))
# limited to 2:297 to match differenced time series
tsWeeklyAdj = xts(tsWeeklyAdj[2:297,])

# ADF and KPSS tests on differenced data
adfDiff = adf.test(tsWeeklyDiff, alternative = "stationary")
adfDiff

kpssDiff = kpss.test(tsWeeklyDiff, null = "Level")
kpssDiff

# build some ARIMA models using the original and pre-differenced data
arimaAdj = auto.arima(tsWeeklyAdj, allowmean = TRUE)   
summary(arimaAdj)

arimaDiff = arima(tsWeeklyDiff, order = c(3,0,2), include.mean = TRUE)
summary(arimaDiff)

# Forecast points
arimaAdjPred = forecast(arimaAdj, h = 10)
arimaDiffPred = forecast(arimaDiff, h = 10)

# Plot fits:
ts.plot(tsWeeklyAdj, arimaAdjPred$fitted, arimaDiffPred$fitted,
        gpars = list(col = c("black", "green4", "blue2"), 
                     lwd = 3.0, cex.axis = 1.2, xlab = ""))
title(xlab = "Time", ylab = "Manuscripts Received", 
      main = "ARIMA Fits on Weekly Manuscript Volume Data", 
      cex.lab = 1.3, cex.main = 1.5)
grid(nx = NULL, ny = NULL, col = "gray45", lty = "dotted",
     lwd = 1.0)
legend("bottomleft", inset=c(0,0), xpd = TRUE, legend = c("Orig. Time Series", "ARIMA - AutoDiff", 
                                                          "ARIMA - PreDiff"), 
       col = c("black", "green4", "blue2"), 
       lty = 1,lwd = 3.0, title = "Legend", cex = 1.0)
lines(arimaAdjPred$mean, col="green4", type = "o", pch=19, cex = 1.2)
lines(arimaDiffPred$mean, col="blue2", type = "o", pch=19, cex = 1.2)

# Same plot but zoom in on the end of the series
ts.plot(tsWeeklyAdj, arimaAdjPred$fitted, arimaDiffPred$fitted,
        gpars = list(col = c("black", "green4", "blue2"), 
                     lwd = 3.0, cex.axis = 1.2, xlab = "", xlim = c(830, 930)))
title(xlab = "", ylab = "", 
      main = "", 
      cex.lab = 1.3, cex.main = 1.5)
grid(nx = NULL, ny = NULL, col = "gray45", lty = "dotted",
     lwd = 1.0)
lines(arimaAdjPred$mean, col="green4", type = "o", pch=19, cex = 1.2)
lines(arimaDiffPred$mean, col="blue2", type = "o", pch=19, cex = 1.2)
