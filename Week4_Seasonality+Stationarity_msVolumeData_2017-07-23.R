### ~*~*~*~*~*~*           Rebecca Vislay Wade          ~*~*~*~*~*~* ###
### ~*~*~*~*~*~* PREDICT 413: Time Series & Forecasting ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*   Summer 2017 | Section 57 | NU MSPA   ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~ July 23, 2017 *~*~*~*~*~*~*~*~*~*~*~*~*~* ###

### This program examines the decomposition and stationarity of a time
### series. The data is examined at three different windows: the full
### 18 months of daily manuscript volume, 3 months, and 1 month.

# Load some libraries.
library(tseries)
library(forecast)

# Read in from .csv
daily = read.csv("~/NU_MSPA/R_work/PREDICT_413/data/dailyVolumeCleaned.csv", header = TRUE)

# Convert to time series (ts) object, break into 1 and 3 month chunks.
tsDaily = ts(daily$mss)
tsDaily3mos = ts(tsDaily[173:253])        # Oct-Dec 2016
tsDailySep2016 = ts(tsDaily[173:193])     # Sep 2016

# tsdisplay for full dataset
tsdisplay(tsDaily, main = "Daily Manuscript Volume, Full Dataset, Jan 2016 - Jun 2017")

# Create time series with frequency = 5; this aggregates the data into 5 day bins
# 5 days of measurements per unit time
tsDaily_5 = ts(daily$mss, frequency = 5)
tsDaily3mos_5 = ts(tsDaily[173:253], frequency = 5)        # Oct-Dec 2016
tsDailySep2016_5 = ts(tsDaily[173:193], frequency = 5)     # Sep 2016

# Decomposition of entire dataset
decompAll = stl(tsDaily_5, s.window = "periodic")
plot(decompAll, main = "Decomposition of Full Dataset")

# ADF test for stationarity - Full 18 months
adfAll = adf.test(tsDaily_5, alternative = "s")
adfAll

# Decomposition of 3 months of data from Oct-Dec 2016
decomp3mos = stl(tsDaily3mos_5, s.window = "periodic")
plot(decomp3mos, main = "Decomposition of 3 Months of Data (Oct-Dec 2016)")

# ADF test for stationarity - 3 months
adf3mos = adf.test(tsDaily3mos_5, alternative = "s")
adf3mos

# Decomposition of 1 month of data from Sep 2016
decompSep2016 = stl(tsDailySep2016_5, s.window = "periodic")
plot(decompSep2016, main = "Decomposition of 1 Month of Data (Sep 2016)")

# ADF test for stationarity - 3 months
adf1mo = adf.test(tsDailySep2016_5, alternative = "s")
adf1mo