setwd("C:/Users/Pastor/Documents/Courses/Upwork_Projects/forex/VX_fha87ep")
library(lubridate)
library(tidyverse)
library(plyr)
library(plotly)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(tibble)
vix <- read.table('VX_continuous_adjusted_1hour.txt', sep = ",")
vix$V1 <- as.POSIXct(vix$V1, format = "%Y-%m-%d %H:%M:%S")
vix$hour <- hour(vix$V1)
vix$day <- day(vix$V1)
vix$month <- month(vix$V1)
vix$year <- year(vix$V1)
vix <- plyr::rename(vix, c("V1"= "date","V2"= "open", "V3"="high", "V4" = "low", "V5" = "close", "V6" = "volume"))

# return
prices <- vix[, "close", drop = FALSE]
n <- nrow(prices)
ret <- ((prices[2:n, 1] - prices[1:(n-1), 1])/prices[1:(n-1), 1])
vix_ret <- vix[-1,]
vix_ret$ret <- ret
ret_hour <- with(vix_ret, tapply(ret, factor(hour), mean))
ret_hour_sd <- with(vix_ret, tapply(ret, factor(hour), sd))
returns_hour <- data.frame(ret_hour, ret_hour_sd)

g <- ggplot(returns_hour, aes(ret_hour_sd, ret_hour, label = rownames(returns_hour)))+
  geom_point()
ggplotly(g)

## Volume

volume_hour <- with(vix_ret, tapply(volume, factor(hour), mean))
volume_hour_sd <- with(vix_ret, tapply(volume, factor(hour), sd))
returns_hour_v <- data.frame(volume_hour, volume_hour_sd)

g <- ggplot(returns_hour_v, aes(volume_hour_sd, volume_hour, label = rownames(returns_hour_v)))+
  geom_point()
ggplotly(g)


vix_ret_3 <- vix_ret
us30_close = vix_ret_3$close
plot(us30_close)
par(mfrow=c(1,2))
Acf(us30_close, main = "ACF for Differenced Series")
Pacf(us30_close, main = "PACF for Differenced series")
print(adf.test(us30_close)) # we check to see if the data is stacionary this means that the values don't go up and down just don't grow

fitA = auto.arima(us30_close, seasonal = FALSE)
tsdisplay(residuals(fitA), lag.max = 40, main = '(1,1,0) Model Residuals')

term <- 72 # the prediction lenght (I want to predict 100 days)
fcast1 <- forecast(fitA, h = term)
plot(fcast1)

accuracy(fcast1)
fcast1
