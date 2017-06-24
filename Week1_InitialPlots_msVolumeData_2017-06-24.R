### ~*~*~*~*~*~*           Rebecca Vislay Wade          ~*~*~*~*~*~* ###
### ~*~*~*~*~*~* PREDICT 413: Time Series & Forecasting ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*   Summer 2017 | Section 57 | NU MSPA   ~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* ###
### ~*~*~*~*~*~*~*~*~*~*~*~ June 24, 2017 ~*~*~*~*~*~*~*~*~*~*~*~*~* ###

### This program produces two different kinds of plots of time series
### data consisting of the daily number of scientific manuscripts 
### entering an editorial workflow. The first is a basic time series
### plot using plot.ts(). The second uses ggplot2. Two additional
### ggplot2 plots show 6 month and 1 month subsets.

# Read in from .csv
daily <- read.csv("dailyVolumeCleaned.csv", header = TRUE)

# Convert to time series (ts) object
tsDaily <- ts(daily$mss)

# PLOT 01: Basic time series plot
plot.ts(daily$mss)

# But that's not as fun as using ggplot2 so let's convert the dates to
# real dates:
daily$date <- as.Date(daily$date, origin = "1899-12-30")

# NOTE: "origin = " statement is needed to convert Windows Excel text
# dates to date format

# Libraries for plotting
library(ggplot2)
library(ggthemes) # Because different themes are fun!
library(scales) # Allows specification of axis scales, breaks, and formats

# PLOT 02: Basic line plot of the entire 371 data points
ggplot(data=daily, aes(x=date, y=mss, group=1)) +
  geom_line(color = "forestgreen", size = 1) +
  # Plot and title labels
  xlab("Date") +
  ylab("Manuscripts Received") +
  ggtitle("Daily Manuscript Volume - 1/1/2016 to 6/16/2017") +
  # "The Economist" magazine theme
  theme_economist() +
  # Here we adjust all the font sizes, angles, and justifications in a 
  # theme() call.
  theme(axis.text.x=element_text(size = 12, vjust = 1.0, hjust = 1.0, angle = 45),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16, vjust = 2),
        axis.title = element_text(size = 20)) +
  # Use scale_*_* functions from the scales package to set axes scales
  scale_y_continuous(limits = c(0,300), breaks=seq(0,300,100)) + 
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%Y-%m-%d"))
# Now, just the month of August 2016
daily_Aug2016 <- daily[150:172,]

# PLOT 03: Same plot as before except just three months:
ggplot(data=daily_Aug2016, aes(x=date, y=mss, group=1)) +
  geom_line(color = "forestgreen", size = 1) +
  # Plot and title labels
  xlab("Date") +
  ylab("Manuscripts Received") +
  ggtitle("Daily Manuscript Volume - August 2016") +
  # "The Economist" magazine theme
  theme_economist() +
  # Here we adjust all the font sizes, angles, and justifications in a 
  # theme() call.
  theme(axis.text.x=element_text(size = 14, vjust = 1.0, hjust = 1.0, angle = 45),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, vjust = 2),
        axis.title = element_text(size = 22)) +
  scale_y_continuous(limits = c(0,300), breaks=seq(0,300,100)) + 
  scale_x_date(breaks = date_breaks("2 days"), labels = date_format("%Y-%m-%d"))
