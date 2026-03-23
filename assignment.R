library(xts)
library(tseries)

# Data acquisition and setup
moo               <- read.csv("cattle_futures.csv")
index             <- as.Date(moo[[1]], format = "%m/%d/%Y %H:%M:%S")
cow_prices        <- as.numeric(moo[[2]]) |> unname()
cattle            <- xts(cow_prices, order.by = index)
colnames(cattle)  <- "daily"
plot(cattle,
     main = "Live Cattle Futures (SPGSLC) daily spot prices",
     major.ticks = "years",
     minor.ticks = FALSE,
     format.labels = "%Y")

# Log the returns
ln_cattle <- log(cattle)

# Describe the time series:
# 4264 observations from Jan 16 2009 to Mar 04 2026
# Clearly non-stationary
# 4 phases:
# 2009 - late 2014: steady rise
# 2015 - mid 2016: drop
# mid 2016 - Apr 2020: rangebound
# Apr 2020 - end: rapid upward trend
# Look for major economic events - COVID; flash crash. What changed in 2014?

# Clearly non-stationary
acf(ln_cattle, main = "ACF of log(cattle)") # Very obviously non-stationary
adf.test(ln_cattle) # p = 0.958, no evidence against stationarity

# Diff series
ln_cattle_d <- diff(ln_cattle) |> na.omit()

# Stationarity of diffed series
plot(ln_cattle_d)
# Looks pretty stationary, but let's check with Dickey-Fuller
adf.test(ln_cattle_d) # p < 0.01, accept stationarity

# Descriptive stats
library(TSA)

hist(ln_cattle_d, breaks = 50)
mean_ln_r <- mean(ln_cattle_d) # 0.0002345471
var_ln_r <- var(ln_cattle_d) # 0.0001019347
skewness_ln_r <- skewness(ln_cattle_d) # -0.1720272
x_kurtosis_ln_r <- kurtosis(ln_cattle_d) # 2.30418

# Test for normality
x <- rnorm(length(ln_cattle_d), mean_ln_r, sqrt(var_ln_r))
ks.test(coredata(ln_cattle_d), x) # p-value 7.661e-07

qqnorm(ln_cattle_d)
qqline(x, col = "red", lwd = 2)

# TODO: VaR calculations
library(PerformanceAnalytics)
VaR(ln_cattle_d, p = 0.95, type = "historical") #-0.01639114
VaR(ln_cattle_d, p = 0.99, type = "historical") #-0.02985331

# Look at ACF and PACF to determine if it's white noise, and the type of model
# that may be applicable
par(mfrow = c(2, 2))
a <- acf(ln_cattle_d,  plot = FALSE)
# \theta for lag 1 = 0.079
plot(a, ylim = c(-0.2, 0.2), main = "ACF") # acf doesn't use the ylim keyword.
p <- pacf(ln_cattle_d, ylim = c(-0.2, 0.2), main = "PACF")
# \phi for lag 1 = 0.079

# ACF and PACF are almost identical, but with a slightly significant
# autocorrelation at lag 1. Try AR(1), MA(1), and ARIMA(1,1,1)

library(forecast)
par(mfrow = c(1, 1))
c <- as.numeric(cattle)

ar <- Arima(c, c(1, 1, 0))
ar_fc <- forecast(ar, h = 12)
plot(ar_fc, include = 100)

ma <- Arima(c, c(0, 1, 1))
ma_fc <- forecast(ma, h = 12)
plot(ma_fc, include = 100)

arima <- Arima(c, c(1, 1, 1))
fc <- forecast(arima, h = 12)
plot(fc, include = 100)
# Rubbish prediction/model. Let's look at the residuals
ar_res <- residuals(ar)
ma_res <- residuals(ma)

# Look at residuals
par(mfrow = c(2, 3))
arima_res <- residuals(arima)
plot(arima_res)
# The plot appears stationary, except that the variance seems to grow over time
adf.test(arima_res) # p < 0.01, accept stationarity
pacf(arima_res, main = "PACF of Residuals")
acf(arima_res, main = "ACF of Residuals")

var(arima_res) # 20.85732
mean(arima_res) # 0.1157353
hist(arima_res, breaks = 50)

# The residuals look like white noise, but need to double check with Ljung-Box
Box.test(arima_res, type = "Ljung-Box") # p = 0.8434, accept white noise.

# Check for seasonality
stl(ln_cattle_d[-1], s.window = "seasonal") # so unseasonal it throws an error!

# Try and fit a garch model. We need the PACF of the squared series to estimate
# the order.
res_squared <- arima_res^2
# check res_squared isn't white noise
Box.test(res_squared, type = "Ljung-Box")
pacf(res_squared) # p-value < 2.2e-16; reject white noise
# Try a GARCH model
# library(rugarch)
