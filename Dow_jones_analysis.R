setwd("C:/Users/Pastor/Documents/Courses/Upwork_Projects/forex/YM_biqgzjm (1)")
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
YM <- read.table('YM_continuous_adjusted_1hour.txt', sep = ",")
YM$V1 <- as.POSIXct(YM$V1, format = "%Y-%m-%d %H:%M:%S")
YM$hour <- hour(YM$V1)
YM$day <- day(YM$V1)
YM$month <- month(YM$V1)
YM$year <- year(YM$V1)
YM <- plyr::rename(YM, c("V1"= "date","V2"= "open", "V3"="high", "V4" = "low", "V5" = "close", "V6" = "volume"))

# return
prices <- YM[, "close", drop = FALSE]
n <- nrow(prices)
ret <- ((prices[2:n, 1] - prices[1:(n-1), 1])/prices[1:(n-1), 1])
YM_ret <- YM[-1,]
YM_ret$ret <- ret
ret_hour <- with(YM_ret, tapply(ret, factor(hour), mean))
ret_hour_sd <- with(YM_ret, tapply(ret, factor(hour), sd))
returns_hour <- data.frame(ret_hour, ret_hour_sd)

ggplot(returns_hour, aes(ret_hour_sd, ret_hour, label = rownames(returns_hour)))+
  geom_text()

## Volume

volume_hour <- with(YM_ret, tapply(volume, factor(hour), mean))
volume_hour_sd <- with(YM_ret, tapply(volume, factor(hour), sd))
returns_hour_v <- data.frame(volume_hour, volume_hour_sd)

ggplot(returns_hour_v, aes(volume_hour_sd, volume_hour, label = rownames(returns_hour_v)))+
  geom_text()

library(data.table)
df<- YM

average.by.hour.by.reference <- function( df, hrs=16 ) {
  
  df <- as.data.table(df)
  df[, date_number := as.numeric(as.Date(ymd_h( sprintf("%d-%d-%dT%d",year,month,day,hour) ) - hours(hrs))) ]
  df[, delta := close - close[ hour == hrs ], .(date_number) ]
  
  return( df[, .(meanPerHour = mean(delta,na.rm=TRUE)), .(hour) ] )
  
}

hour_16 <- average.by.hour.by.reference( df, 16 ) # produces the above results
ggplot(hour_16, aes(x=hour, y = meanPerHour))+
  geom_point()

YM_ret_3 <- YM_ret
us30_close = YM_ret_3$close
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
