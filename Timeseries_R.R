data <- read.csv("C:/Users/BEHINLAPTOP/Desktop/ts.csv")
data
library(forcats)
myts <- ts(data$AQI, frequency=365, start=c(1401,01 ,01),end=c(1401,06,31))

myts2 <- as.ts(ts(myts, start=c(1401,01 ,01), end=c(1401,06,31)))



arimadata=auto.arima(myts)

forecast(arimadata,h=181)
## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("1401/01/01"), as.Date("1401/12/30"), by = "day")

## Create a time series object
set.seed(25)
myts <- ts(rnorm(length(inds)),     # random data
           start = c(1401, as.numeric(format(inds[1], "%j"))),
           frequency = 365)


as.numeric(format(inds[1], "%j"))


## use auto.arima to choose ARIMA terms
fit <- auto.arima(myts)
## forecast for next 60 time points
fore <- forecast(fit, h = 60)
## plot it
plot(fore)


## create the zoo object as before
set.seed(25)
myzoo <- zoo(rnorm(length(inds)), inds)


## use auto.arima to choose ARIMA terms
fit <- auto.arima(myts)
## forecast for next 60 time points
fore <- forecast(fit, h = 1)



## plot it
plot(fore, xaxt = "n")    # no x-axis 
Axis(inds, side = 1)



## plot it
plot(fore, xaxt = "n")    # no x-axis 
Axis(inds, side = 1,
     at = seq(inds[1], tail(inds, 1) + 60, by = "3 months"),
     format = "%b %Y")


library(zoo)
zoo(data$AQI, seq(from = as.Date("1401/07/01"), to = as.Date("1401/12/30"), by = 1))


library(lubridate)
set.seed(42)
minday = as.Date("1401/07/01")
maxday = as.Date("1401/12/30")
dates <- seq(minday, maxday, "days")
dates <- dates[sample(1:length(dates),length(dates)/4)] # create some holes
df <- data.frame(date=sort(dates), val=sin(seq(from=0, to=2*pi, length=length(dates))))
df

df <- merge(df, data.frame(date=seq(minday, maxday, "days")), all=T)


nts <- ts(df$val, frequency=365, start=c(year(minday), as.numeric(format(minday, "%j"))))
plot(nts)