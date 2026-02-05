library(testthat)

# =============================================================================
# WNN Class Tests
# =============================================================================

# --- Test Data Setup ---------------------------------------------------------

# Simple reproducible time series for testing
set.seed(42)
test_ts <- ts(rnorm(200, mean = 100, sd = 10), frequency = 12, start = c(2020, 1))
test_vector <- as.numeric(test_ts)

# Small dataset for edge cases
small_data <- 1:50


# =============================================================================
# INITIALIZATION TESTS
# =============================================================================

test_that("WNN can be created with valid parameters", {

  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_s3_class(wnn, "WNN")
  expect_equal(wnn$get_horizon(), 10)
  expect_equal(wnn$get_window(), 20)
  expect_equal(wnn$get_k(), 3)
})

test_that("WNN rejects missing parameters", {
  expect_error(WNN$new(), "horizon is required")
  expect_error(WNN$new(horizon = 10), "window is required")
  expect_error(WNN$new(horizon = 10, window = 20), "k is required")
})

test_that("WNN rejects non-integer parameters", {
  expect_error(WNN$new(horizon = 10.5, window = 20, k = 3), "must be an integer")
  expect_error(WNN$new(horizon = 10, window = 20.5, k = 3), "must be an integer")
  expect_error(WNN$new(horizon = 10, window = 20, k = 3.5), "must be an integer")
})

test_that("WNN rejects non-positive parameters", {
  expect_error(WNN$new(horizon = 0, window = 20, k = 3), "must be a positive integer")
  expect_error(WNN$new(horizon = -1, window = 20, k = 3), "must be a positive integer")
  expect_error(WNN$new(horizon = 10, window = 0, k = 3), "must be a positive integer")
  expect_error(WNN$new(horizon = 10, window = 20, k = -1), "must be a positive integer")
})

test_that("WNN rejects non-numeric parameters", {
  expect_error(WNN$new(horizon = "10", window = 20, k = 3), "must be a single numeric")
  expect_error(WNN$new(horizon = 10, window = NULL, k = 3), "is required")
})


# =============================================================================
# FIT METHOD TESTS
# =============================================================================

test_that("fit() works with numeric vector", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  result <- wnn$fit(test_vector)

  expect_identical(result, wnn)  # Returns invisible self
  expect_equal(wnn$get_original_data(), test_vector)
})

test_that("fit() works with ts object", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  wnn$fit(test_ts)

  expect_true(is.ts(wnn$get_original_data()))
  expect_equal(length(wnn$get_original_data()), length(test_ts))
})

test_that("fit() rejects NULL input", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_error(wnn$fit(NULL), "tsdata is required")
})

test_that("fit() rejects non-numeric input", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_error(wnn$fit(c("a", "b", "c")), "must be numeric")
  expect_error(wnn$fit(list(1, 2, 3)), "must be numeric")
})

test_that("fit() rejects too short time series", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  # Need at least w + h = 30 observations

expect_error(wnn$fit(1:29), "Time series too short")
  expect_silent(wnn$fit(1:50))  # 50 >= 30, should work
})

test_that("fit() rejects when not enough data for k neighbors", {
  # With w=20, h=10, k=100, we need at least n - w - h + 1 >= k
  # So n >= k + w + h - 1 = 100 + 20 + 10 - 1 = 129
  wnn <- WNN$new(horizon = 10, window = 20, k = 100)

  expect_error(wnn$fit(1:50), "Not enough data for 100 neighbors")
})

test_that("fit() resets previous results", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 2)

  # First fit and predict
  wnn$fit_predict(small_data)
  expect_false(is.null(wnn$get_forecast()))

  # Second fit should reset
  wnn$fit(small_data)
  expect_null(wnn$get_forecast())
  expect_null(wnn$get_distances())
  expect_null(wnn$get_weights())
  expect_null(wnn$get_nearest_neighbors())
})


# =============================================================================
# PREDICT METHOD TESTS
# =============================================================================

test_that("predict() fails when model not fitted", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_error(wnn$predict(), "Model must be fitted")
})
test_that("predict() works after fit()", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)
  wnn$fit(test_vector)

  forecast <- wnn$predict()

  expect_length(forecast, 10)
  expect_true(is.numeric(forecast))
})

test_that("predict() returns ts object when fitted with ts", {
  wnn <- WNN$new(horizon = 12, window = 24, k = 3)
  wnn$fit(test_ts)

  forecast <- wnn$predict()

  expect_true(is.ts(forecast))
  expect_equal(frequency(forecast), 12)
  expect_length(forecast, 12)
})

test_that("predict() accepts custom pattern", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 2)
  wnn$fit(small_data)

  custom_pattern <- rep(25, 10)
  forecast <- wnn$predict(pattern = custom_pattern)

  expect_length(forecast, 5)
})

test_that("predict() rejects pattern with wrong length", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 2)
  wnn$fit(small_data)

  expect_error(wnn$predict(pattern = 1:5), "Pattern must have length w = 10")
  expect_error(wnn$predict(pattern = 1:15), "Pattern must have length w = 10")
})

test_that("predict() stores nearest neighbors information", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 3)
  wnn$fit(small_data)
  wnn$predict()

  neighbors <- wnn$get_nearest_neighbors()

  expect_type(neighbors, "list")
  expect_named(neighbors, c("indices", "distances", "weights", "future_values"))
  expect_length(neighbors$indices, 3)
  expect_length(neighbors$distances, 3)
  expect_length(neighbors$weights, 3)
  expect_length(neighbors$future_values, 3)
})

test_that("predict() returns neighbors sorted by distance", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 3)
  wnn$fit(small_data)
  wnn$predict()

  distances <- wnn$get_distances()

  # Should be sorted in ascending order
  expect_equal(distances, sort(distances))
})


# =============================================================================
# FIT_PREDICT METHOD TESTS
# =============================================================================

test_that("fit_predict() combines fit and predict", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  forecast <- wnn$fit_predict(test_vector)

  expect_length(forecast, 10)
  expect_equal(wnn$get_original_data(), test_vector)
  expect_false(is.null(wnn$get_nearest_neighbors()))
})

test_that("fit_predict() gives same result as fit() then predict()", {
  wnn1 <- WNN$new(horizon = 5, window = 10, k = 2)
  wnn2 <- WNN$new(horizon = 5, window = 10, k = 2)

  forecast1 <- wnn1$fit_predict(small_data)

  wnn2$fit(small_data)
  forecast2 <- wnn2$predict()

  expect_equal(forecast1, forecast2)
})


# =============================================================================
# GETTER TESTS
# =============================================================================

test_that("getters return correct hyperparameters", {
  wnn <- WNN$new(horizon = 12, window = 24, k = 5)

  expect_equal(wnn$get_horizon(), 12)
  expect_equal(wnn$get_window(), 24)
  expect_equal(wnn$get_k(), 5)
})

test_that("getters return NULL before fitting", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_null(wnn$get_original_data())
  expect_null(wnn$get_forecast())
  expect_null(wnn$get_distances())
  expect_null(wnn$get_weights())
  expect_null(wnn$get_nearest_neighbors())
})

test_that("get_weights() returns normalized weights", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 3)
  wnn$fit_predict(small_data)

  weights <- wnn$get_weights()

  expect_equal(sum(weights), 1, tolerance = 1e-10)
  expect_true(all(weights > 0))
})


# =============================================================================
# ALGORITHM CORRECTNESS TESTS
# =============================================================================

test_that("WNN finds correct nearest neighbors for simple pattern", {
  # Create data with a clear repeating pattern
  # Pattern [1,2,3,4,5] appears at positions 1-5, 11-15, 21-25
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            1, 2, 3, 4, 5)  # Last 5 values as pattern

  wnn <- WNN$new(horizon = 5, window = 5, k = 2)
  wnn$fit_predict(data)

  neighbors <- wnn$get_nearest_neighbors()

  # The pattern [1,2,3,4,5] should match exactly at indices 1, 11, 21
  # Expected indices are 1, 11, or 21 (positions where [1,2,3,4,5] starts)
  expect_true(all(neighbors$indices %in% c(1, 11, 21)))

  # With exact matches, distances should be 0 (or very close due to epsilon)
  expect_true(all(neighbors$distances < 1e-5))
})

test_that("WNN weighted average is computed correctly", {
  # Simple case: if all neighbors have same distance, forecast = simple average
  # Create pattern that appears once
  wnn <- WNN$new(horizon = 2, window = 3, k = 2)

  # Data: [10, 20, 30, 100, 200,  10, 20, 30, 50, 60]
  #        CCj1             Cj1   CCj2             Cj2
  # If CCi = [10, 20, 30], both CCj1 and CCj2 match exactly
  # Future values: Cj1 = [100, 200], Cj2 = [50, 60]
  # With equal weights: forecast = ([100,200] + [50,60]) / 2 = [75, 130]

  data <- c(10, 20, 30, 100, 200, 10, 20, 30, 50, 60, 10, 20, 30)
  wnn$fit_predict(data)

  forecast <- wnn$get_forecast()

  # With equal distances (both ~0), weights should be ~equal
  # So forecast should be approximately the average
  expect_equal(forecast[1], 75, tolerance = 1)
  expect_equal(forecast[2], 130, tolerance = 1)
})

test_that("closer neighbors have higher weights", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 3)
  wnn$fit_predict(test_vector)

  weights <- wnn$get_weights()
  distances <- wnn$get_distances()

  # First neighbor (smallest distance) should have highest weight
  expect_equal(which.max(weights), 1)
  expect_equal(which.min(distances), 1)
})


# =============================================================================
# PRINT AND SUMMARY TESTS
# =============================================================================

test_that("print() works before fitting", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_output(print(wnn), "WNN Model")
  expect_output(print(wnn), "Horizon")
  expect_output(print(wnn), "Not fitted")
})

test_that("print() works after fitting", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 2)
  wnn$fit_predict(small_data)

  expect_output(print(wnn), "WNN Model")
  expect_output(print(wnn), "Fitted")
})

test_that("summary() works before fitting", {
  wnn <- WNN$new(horizon = 10, window = 20, k = 3)

  expect_output(wnn$summary(), "HYPERPARAMETERS")
  expect_output(wnn$summary(), "Model not fitted")
})

test_that("summary() displays complete information after fitting", {
  wnn <- WNN$new(horizon = 5, window = 10, k = 2)
  wnn$fit_predict(small_data)

  expect_output(wnn$summary(), "ORIGINAL DATA")
  expect_output(wnn$summary(), "NEAREST NEIGHBORS")
  expect_output(wnn$summary(), "FORECAST")
})


# =============================================================================
# TIME SERIES CONTINUATION TESTS
# =============================================================================

test_that("forecast ts continues from original ts end", {
  # Original ts ends at 2020-12 (month 200 / 12 = 16.67, so 2020 + 16 = 2036, month 8)
  # Actually: start = c(2020, 1), length = 200, freq = 12
  # End = 2020 + (200-1)/12 = 2020 + 16.58 = 2036, month 7+1 = 8
  # So end is c(2036, 8), forecast should start at c(2036, 9)

  wnn <- WNN$new(horizon = 12, window = 24, k = 3)
  wnn$fit_predict(test_ts)

  forecast <- wnn$get_forecast()
  original_end <- end(test_ts)
  forecast_start <- start(forecast)

  # Forecast should start right after original ends
  expect_equal(frequency(forecast), frequency(test_ts))

  # Check that forecast starts one period after original ends
  if (original_end[2] == 12) {
    expect_equal(forecast_start, c(original_end[1] + 1, 1))
  } else {
    expect_equal(forecast_start, c(original_end[1], original_end[2] + 1))
  }
})
