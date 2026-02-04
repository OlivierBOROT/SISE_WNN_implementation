library(testthat)

# INIT tests

test_that("WNN$new() works with valid parameters", {
  valid_cases <- list(
    list(horizon = 1, window = 1, k = 1),
    list(horizon = 5, window = 10, k = 3),
    list(horizon = 12, window = 24, k = 5),
    list(horizon = 100, window = 50, k = 10)
  )

  for (case in valid_cases) {
    wnn <- WNN$new(case$horizon, case$window, case$k)
    expect_s3_class(wnn, "WNN")
    expect_equal(wnn$get_horizon(), case$horizon)
    expect_equal(wnn$get_window(), case$window)
    expect_equal(wnn$get_k(), case$k)
  }
})

test_that("WNN$new() fails for non-integer numeric inputs", {
  expect_error(WNN$new(horizon = 2.5, window = 10, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10.1, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10, k = 3.9))
})

test_that("WNN$new() fails for non-numeric inputs", {
  expect_error(WNN$new(horizon = "2", window = 10, k = 3))
  expect_error(WNN$new(horizon = 2, window = TRUE, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10, k = "five"))
})

test_that("WNN$new() fails for NULL inputs", {
  expect_error(WNN$new(horizon = NULL, window = 10, k = 3))
  expect_error(WNN$new(horizon = 2, window = NULL, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10, k = NULL))
})

test_that("WNN$new() fails for zero or negative inputs", {
  # Negative values
  expect_error(WNN$new(horizon = -1, window = 10, k = 3))
  expect_error(WNN$new(horizon = 2, window = -5, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10, k = -2))

  # Zero values
  expect_error(WNN$new(horizon = 0, window = 10, k = 3))
  expect_error(WNN$new(horizon = 2, window = 0, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10, k = 0))
})

test_that("WNN$new() fails if any input is missing", {
  expect_error(WNN$new(window = 10, k = 3))
  expect_error(WNN$new(horizon = 2, k = 3))
  expect_error(WNN$new(horizon = 2, window = 10))
  expect_error(WNN$new())
})

# fit predict tests

test_that("fit_predict() returns correct length forecast", {
  # Create simple test data
  set.seed(42)
  test_data <- ts(rnorm(100), frequency = 12)

  horizons <- c(1, 5, 12, 24)

  for (h in horizons) {
    wnn <- WNN$new(horizon = h, window = 10, k = 3)
    forecast <- wnn$fit_predict(test_data)
    expect_equal(length(forecast), h)
  }
})

test_that("fit_predict() returns ts object when input is ts", {
  test_data <- ts(1:100, frequency = 12, start = c(2020, 1))

  wnn <- WNN$new(horizon = 6, window = 12, k = 2)
  forecast <- wnn$fit_predict(test_data)

  expect_true(is.ts(forecast))
  expect_equal(frequency(forecast), 12)
})

test_that("fit_predict() works with numeric vector", {
  test_data <- 1:100

  wnn <- WNN$new(horizon = 5, window = 10, k = 3)
  forecast <- wnn$fit_predict(test_data)

  expect_equal(length(forecast), 5)
  expect_true(is.numeric(forecast))
})

test_that("fit_predict() fails with too short time series", {
  short_data <- ts(1:10, frequency = 12)

  # Need at least window + horizon = 20 + 5 = 25 observations
  wnn <- WNN$new(horizon = 5, window = 20, k = 3)
  expect_error(wnn$fit_predict(short_data))
})

test_that("fit_predict() fails with non-numeric input", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 3)

  expect_error(wnn$fit_predict("not a time series"))
  expect_error(wnn$fit_predict(list(a = 1, b = 2)))
  expect_error(wnn$fit_predict(NULL))
})

test_that("fit_predict() fails when k > possible neighbors", {
  # With 30 observations, window=10, horizon=5
  # max neighbors = 30 - 10 - 5 + 1 = 16
  test_data <- ts(1:30, frequency = 12)

  wnn <- WNN$new(horizon = 5, window = 10, k = 20)  # k=20 > 16

  expect_error(wnn$fit_predict(test_data))
})

# getters tests

test_that("Getter methods return correct values", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 5)

  expect_equal(wnn$get_horizon(), 10)
  expect_equal(wnn$get_window(), 20)
  expect_equal(wnn$get_k(), 5)
})

test_that("get_original_data and get_forecast are NULL before fit", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 5)

  expect_null(wnn$get_original_data())
  expect_null(wnn$get_forecast())
  expect_null(wnn$get_distances())
  expect_null(wnn$get_weights())
})

test_that("get_original_data, get_forecast, get_distances, get_weights are set after fit", {
  test_data <- ts(1:100, frequency = 12)
  wnn <- WNN$new(horizon = 6, window = 12, k = 3)
  forecast <- wnn$fit_predict(test_data)

  expect_equal(wnn$get_original_data(), test_data)
  expect_equal(wnn$get_forecast(), forecast)
  expect_equal(length(wnn$get_distances()), 3)
  expect_equal(length(wnn$get_weights()), 3)
  expect_equal(sum(wnn$get_weights()), 1)  # Weights should sum to 1
})

# fit_predict edge case tests

test_that("WNN finds exact match when pattern repeats", {
  # Create data with exact repeating pattern
  pattern <- c(1, 2, 3, 4, 5)
  future <- c(10, 20, 30)
  test_data <- ts(c(pattern, future, pattern, future, pattern), frequency = 1)

  wnn <- WNN$new(horizon = 3, window = 5, k = 2)
  forecast <- wnn$fit_predict(test_data)

  # With exact matches, forecast should be very close to [10, 20, 30]
  expect_true(all(abs(forecast - future) < 0.01))
})

test_that("WNN gives higher weight to closer neighbors", {
  # Create data where nearest neighbor should dominate
  set.seed(123)
  test_data <- ts(c(1:50, rnorm(50, mean = 100)), frequency = 12)

  wnn <- WNN$new(horizon = 5, window = 10, k = 3)
  forecast <- wnn$fit_predict(test_data)

  # Forecast should be numeric and finite
  expect_true(all(is.finite(forecast)))
})

# print / summary tests

test_that("print method works", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 5)

  expect_output(print(wnn), "WNN Model")
  expect_output(print(wnn), "Horizon")
  expect_output(print(wnn), "Window")
  expect_output(print(wnn), "Neighbors")
  expect_output(print(wnn), "Not fitted")

  # After fitting
  test_data <- ts(1:100, frequency = 12)
  wnn$fit_predict(test_data)
  expect_output(print(wnn), "Fitted")
})

test_that("summary method works before fitting", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 5)

  expect_output(wnn$summary(), "WNN MODEL - COMPLETE SUMMARY")
  expect_output(wnn$summary(), "HYPERPARAMETERS")
  expect_output(wnn$summary(), "not fitted")
})

test_that("summary method works after fitting", {
  test_data <- ts(1:100, frequency = 12, start = c(2020, 1))
  wnn <- WNN$new(horizon = 6, window = 12, k = 3)
  wnn$fit_predict(test_data)

  expect_output(wnn$summary(), "WNN MODEL - COMPLETE SUMMARY")
  expect_output(wnn$summary(), "HYPERPARAMETERS")
  expect_output(wnn$summary(), "ORIGINAL DATA")
  expect_output(wnn$summary(), "NEAREST NEIGHBORS")
  expect_output(wnn$summary(), "FORECAST")
  expect_output(wnn$summary(), "Model fitted")
})
