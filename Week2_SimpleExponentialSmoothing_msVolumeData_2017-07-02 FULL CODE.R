### ~*~*~*~*~*~*           Rebecca Vislay Wade          ~*~*~*~*~*~* ###
### ~*~*~*~*~*~* PREDICT 413: Time Series & Forecasting ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*   Summer 2017 | Section 57 | NU MSPA   ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~ July 2, 2017 ~*~*~*~*~*~*~*~*~*~*~*~*~* ###

### This program demonstrates some simple moving average and exponential
### smoothing methods with SMA() from the TTR package and ses() from the
### forecast package, respectively. Included is a custom function called
### sesExperiment that performs SES for three different alpha values and
### plots the fits and residuals overlaid with the original data.

# Load some libraries.
library(TTR)
library(forecast)

# Read in from .csv
daily = read.csv("~/NU_MSPA/R_work/PREDICT_413/data/dailyVolumeCleaned.csv", header = TRUE)

# Convert to time series (ts) object, break into 1, 3, and 6 month chunks.
tsDaily = ts(daily$mss)
tsDaily6mos = ts(tsDaily[130:253])        # Jul-Dec 2016
tsDaily3mos = ts(tsDaily[173:253])        # Oct-Dec 2016
tsDailySep2016 = ts(tsDaily[173:193])     # Sep 2016

# First, a simple moving average with SMA() from the TTR library.
smaSmooth = SMA(tsDaily6mos, n = 5)
ts.plot(tsDaily6mos, smaSmooth, gpars = list(col = c("black", "red")))

# Function to try different alphas and data subsets with ses().
# Arguments: data = your time series as a ts object
#            alphas = vector (numeric) of 3 different alphas to try
#            colors = vector (strings) of colors for the plot
# Output: Two panel plot of fits and residuals.
# Original time series is always plotted in black.
sesExperiment = function(data, alphas, colors){
  ses01 = ses(data, alpha = alphas[1], initial = "simple")
  ses02 = ses(data, alpha = alphas[2], initial = "simple")
  ses03 = ses(data, alpha = alphas[3], initial = "simple")
  # Plot setup:
  par(mfrow=c(2,1), mar=c(5.2, 4.2, 4.2, 8.2))
  # Plot fits:
  ts.plot(data, ses01$fitted, ses02$fitted, ses03$fitted,
          gpars = list(col = c("black", colors), lwd = rep(3.0, 4), xaxt = "n",
                       yaxt = "n"),
          xlab = "")
  axis(1, cex.axis = 1.2)
  axis(2, cex.axis = 1.2)
  title(xlab = "Days", ylab = "Manuscripts Received", 
        main = "SES Fits", 
        cex.lab = 1.3, cex.main = 1.5)
  grid(nx = NULL, ny = NULL, col = "gray45", lty = "dotted",
       lwd = 1.0)
  legend("bottomright", inset=c(-0.2,0), xpd = TRUE, legend = alphas, col = colors, lty = 1,
         lwd = 3.0, title = "Alpha", cex = 1.0)
  # Plot residuals:
  ts.plot(data, ses01$residuals, ses02$residuals, ses03$residuals,
          gpars = list(col = c("black", colors), lwd = rep(3.0, 4), xaxt = "n",
                       yaxt = "n"),
          xlab = "")
  axis(1, cex.axis = 1.2)
  axis(2, cex.axis = 1.2)
  title(xlab = "Days", ylab = "Manuscripts Received", 
        main = "SES Residuals", 
        cex.lab = 1.3, cex.main = 1.5)
  grid(nx = NULL, ny = NULL, col = "gray45", lty = "dotted",
       lwd = 1.0)
  legend("bottomright", inset = c(-0.2,0), xpd = TRUE, legend = alphas, col = colors, lty = 1,
         lwd = 3.0, title = "Alpha", cex = 1.0)
}

# Experiment #1: 6 months of data (Jul-Dec 2016), alpha = 0.1, 0.3, 0.5
sesExperiment(tsDaily6mos, alphas = c(0.5, 0.3, 0.1), 
              colors = c("green4", "deepskyblue", "firebrick"))

# Experiment #2: 3 months of data (Oct-Dec 2016), alpha = 0.1, 0.3, 0.5
sesExperiment(tsDaily3mos, alphas = c(0.5, 0.3, 0.1), 
              colors = c("green4", "deepskyblue", "firebrick"))

# Experiment #3: 3 months of data (Oct-Dec 2016), alpha = 0.2, 0.3, 0.4
sesExperiment(tsDaily3mos, alphas = c(0.4, 0.3, 0.2), 
              colors = c("green4", "deepskyblue", "firebrick"))

