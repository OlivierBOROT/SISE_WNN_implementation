## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(OlivierBorot)
library(forecast)
library(ggplot2)

## ----data-exploration---------------------------------------------------------
# Load the AirPassengers dataset
data(AirPassengers)

# Basic information
cat("Dataset: AirPassengers\n")
cat("Length:", length(AirPassengers), "observations\n")
cat("Frequency:", frequency(AirPassengers), "(monthly)\n")
cat("Period:", paste(start(AirPassengers), collapse = "-"), "to",
    paste(end(AirPassengers), collapse = "-"), "\n")

# Plot the data
autoplot(AirPassengers) +
  ggtitle("Monthly International Airline Passengers (1949-1960)") +
  xlab("Year") +
  ylab("Passengers (thousands)")

## ----train-test-split---------------------------------------------------------
# Split data
train_end <- c(1959, 12)
train <- window(AirPassengers, end = train_end)
test <- window(AirPassengers, start = c(1960, 1))

cat("Training set:", length(train), "observations\n")
cat("Test set:", length(test), "observations\n")

# Plot split
autoplot(train) +
  autolayer(test, series = "Test") +
  ggtitle("Train-Test Split") +
  xlab("Year") +
  ylab("Passengers")

## ----wnn-model----------------------------------------------------------------
# Create WNN model
# - horizon = 12 (predict 12 months)
# - window = 24 (use 24 months for pattern matching)
# - k = 5 (use 5 nearest neighbors)
wnn_model <- WNN$new(horizon = 12, window = 24, k = 5)

# Display model parameters
print(wnn_model)

# Fit and predict
wnn_forecast <- wnn_model$fit_predict(train)

# Display complete summary
wnn_model$summary()

## ----visualization------------------------------------------------------------
# Plot forecast vs actual
autoplot(train) +
  autolayer(test, series = "Actual") +
  autolayer(wnn_forecast, series = "WNN Forecast") +
  ggtitle("WNN Forecast vs Actual Values") +
  xlab("Year") +
  ylab("Passengers") +
  guides(colour = guide_legend(title = "Series"))

## ----accuracy-----------------------------------------------------------------
# Calculate accuracy metrics
errors <- test - wnn_forecast

# Mean Absolute Error (MAE)
mae <- mean(abs(errors))

# Root Mean Square Error (RMSE)
rmse <- sqrt(mean(errors^2))

# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs(errors / test)) * 100

cat("Forecast Accuracy Metrics:\n")
cat("-------------------------\n")
cat("MAE: ", round(mae, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")
cat("MAPE:", round(mape, 2), "%\n")

## ----comparison---------------------------------------------------------------
# ETS model
ets_model <- ets(train)
ets_forecast <- forecast(ets_model, h = 12)

# ARIMA model
arima_model <- auto.arima(train)
arima_forecast <- forecast(arima_model, h = 12)

# Calculate RMSE for each method
rmse_wnn <- sqrt(mean((test - wnn_forecast)^2))
rmse_ets <- sqrt(mean((test - ets_forecast$mean)^2))
rmse_arima <- sqrt(mean((test - arima_forecast$mean)^2))

# Create comparison table
comparison <- data.frame(
  Method = c("WNN (k=5, w=24)", "ETS", "ARIMA"),
  RMSE = round(c(rmse_wnn, rmse_ets, rmse_arima), 2)
)

print(comparison)

## ----comparison-plot----------------------------------------------------------
# Visual comparison
autoplot(train) +
  autolayer(test, series = "Actual") +
  autolayer(wnn_forecast, series = "WNN") +
  autolayer(ets_forecast$mean, series = "ETS") +
  autolayer(arima_forecast$mean, series = "ARIMA") +
  ggtitle("Comparison of Forecasting Methods") +
  xlab("Year") +
  ylab("Passengers") +
  guides(colour = guide_legend(title = "Method"))

## ----hyperparameter-k---------------------------------------------------------
k_values <- c(1, 3, 5, 7, 10)
results <- data.frame(k = integer(), RMSE = numeric())

for (k in k_values) {
  model <- WNN$new(horizon = 12, window = 24, k = k)
  pred <- model$fit_predict(train)
  rmse <- sqrt(mean((test - pred)^2))
  results <- rbind(results, data.frame(k = k, RMSE = rmse))
}

print(results)

ggplot(results, aes(x = k, y = RMSE)) +
  geom_line() +
  geom_point(size = 3) +
  ggtitle("Effect of Number of Neighbors (k) on RMSE") +
  xlab("Number of Neighbors (k)") +
  ylab("RMSE")

## ----hyperparameter-w---------------------------------------------------------
window_values <- c(12, 18, 24, 36, 48)
results_w <- data.frame(window = integer(), RMSE = numeric())

for (w in window_values) {
  model <- WNN$new(horizon = 12, window = w, k = 5)
  pred <- model$fit_predict(train)
  rmse <- sqrt(mean((test - pred)^2))
  results_w <- rbind(results_w, data.frame(window = w, RMSE = rmse))
}

print(results_w)

ggplot(results_w, aes(x = window, y = RMSE)) +
  geom_line() +
  geom_point(size = 3) +
  ggtitle("Effect of Window Size (w) on RMSE") +
  xlab("Window Size (w)") +
  ylab("RMSE")

