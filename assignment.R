library(xts)
library(tseries)

# Data acquisition and setup
moo               <- read.csv("cattle_futures.csv")
index             <- as.Date(moo[[1]], format = "%d/%m/%Y")
cow_prices        <- as.numeric(moo[[2]]) |> unname()
cattle            <- xts(cow_prices, order.by = index)
colnames(cattle)  <- "daily"
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
adf.test(ln_cattle) # p = 0.958, no evidence against stationarity

# Diff series
ln_cattle_d <- diff(ln_cattle)
plot(ln_cattle_d)
ln_cattle_d[1] <- 0

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

# VaR calculations
library(PerformanceAnalytics)

# Stationarity
# Looks pretty stationary, but let's check with Dickey-Fuller
adf.test(ln_cattle_d) # p < 0.01, accept stationarity


par(mfrow = c(2, 2))
# So, if we want to use stationary models we must work with the diffed series.
a <- acf(ln_cattle_d, ylim = c(-1, 1), main = "ACF")
a
p <- pacf(ln_cattle_d, ylim = c(-0.2, 0.2), main = "PACF")

# Merest hint of a significnt lag at k=1.
ma1 <- arima(ln_cattle_d, order = c(0,0,1))
ma1
