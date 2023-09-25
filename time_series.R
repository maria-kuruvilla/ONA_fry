#Goal - plot the atu and peak emergence dates as time series

#look at whether there is a trend in both the time series

library(forecast)

data <- read.csv("../../data/peaks_phenomix_w_covariates.csv")

atu_ts <- ts(data$atu_105, start = 2004, end = 2022, deltat = 1)
plot.ts(atu_ts, ylab = "Accumulated Thermal Units",
        xlab = "Year", lwd = 5, col = "darkblue")
fit <- auto.arima(atu_ts, trace = TRUE)
#does not seem to have a trend

atu_ts <- ts(data$atu, start = 2004, end = 2022, deltat = 1)
plot.ts(atu_ts, ylab = "Accumulated Thermal Units",
        xlab = "Year", lwd = 5, col = "darkblue")
fit <- auto.arima(atu_ts, trace = TRUE)
#has a trend

peak_ts <- ts(data$value, start = 2004, end = 2022, deltat = 1)
plot.ts(peak_ts, ylab = "Accumulated Thermal Units",
        xlab = "Year", lwd = 5, col = "salmon")
auto.arima(peak_ts, trace = TRUE)

#does not seem to have a trend

