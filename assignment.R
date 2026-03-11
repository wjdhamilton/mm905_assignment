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

# Stationarity
# Diff series
ln_cattle_d <- diff(cattle)
ln_cattle_d[1] <- 0
plot(ln_cattle_d)

# Looks pretty stationary, but let's check with Dickey-Fuller
adf.test(ln_cattle) # p = 0.958, no evidence against stationarity
adf.test(ln_cattle_d) # p < 0.01, accept stationarity

# So, if we want to use stationary models we must work with the diffed series.
