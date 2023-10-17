# 1. Load Necessary Libraries
if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("tseries", quietly = TRUE)) {
  install.packages("tseries")
}
library(forecast)
library(ggplot2)
library(tseries)

# Get latest sales info
sales_data <- read.csv("sales_amer_20190201.csv", stringsAsFactors = FALSE)
sales_data$date <- as.Date(sales_data$date, format="%Y-%m-%d") 
sales_ts <- ts(sales_data$sales, frequency = 12) # Monthly data -> adjust

summary(sales_data)


ggplot(sales_data, aes(x = date, y = sales)) + 
  geom_line(color = "blue") + 
  labs(title = "Over Time", x = "Date", y = "Sales") + 
  theme_minimal()

# Decompose
decomposed_sales <- decompose(sales_ts)
plot(decomposed_sales)

#  missing values
missing_dates <- sales_data[is.na(sales_data$sales), "date"]
if(length(missing_dates) > 0) {
  print("Missing Dates:")
  print(missing_dates)
  # Impute 
  sales_data$sales <- na.approx(sales_data$sales)
}

# Stationarity (ADt)
adf_test <- adf.test(sales_ts, alternative = "stationary")
print(adf_test$p.value)
if(adf_test$p.value > 0.05) {
  cat("Not stationary \n")
  # Differencing the series
  sales_ts_diff <- diff(sales_ts)
  plot(sales_ts_diff, main = "Differenced TS")
} else {
  cat("OK.")
}

#  ARIMA
auto_arima <- auto.arima(sales_ts, seasonal = TRUE, stepwise = TRUE, trace = TRUE)
summary(auto_arima)

forecast_sales <- forecast(auto_arima, h = 12)
plot(forecast_sales)

# residuals
residuals <- residuals(auto_arima)
autoplot(residuals) + 
  ggtitle("Residuals of Model 1")

# ACF / PACF 
acf(residuals, main = "ACF")
pacf(residuals, main = "PACF")

# Ljung-Box
lb_test <- Box.test(residuals, lag = log(length(residuals)))
print(lb_test$p.value)
if(lb_test$p.value <= 0.05) {
  cat("Residuals not Independent.")
} else {
  cat("OK")
}

print("Done.")

