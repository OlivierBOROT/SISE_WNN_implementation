#' Weighted Nearest Neighbors (WNN) for Time Series Forecasting
#'
#' The `WNN` class implements a **Weighted Nearest Neighbors** forecasting
#' algorithm for univariate time series, as described in Talavera-Llames et al. (2016).
#'
#' @description
#' The algorithm works as follows:
#' \enumerate{
#'   \item Extract the last \code{w} observations from the time series as the reference pattern (CCi)
#'   \item Slide a window of size \code{w} through the historical data to find similar patterns
#'   \item Compute the Euclidean distance between CCi and each historical pattern (CCj)
#'   \item Select the \code{k} nearest neighbors based on minimum distance
#'   \item Compute a weighted average of the \code{h} values following each neighbor,
#'         where weights are inverse-distance squared: \eqn{\alpha_j = 1 / d_j^2}
#' }
#'
#' @section Mathematical Formulation:
#' Given a time series with values up to \eqn{c_i}, we want to predict the next \eqn{h} values.
#'
#' The reference window is defined as:
#' \deqn{CC_i = [c_{i-w+1}, c_{i-w+2}, \ldots, c_i]}
#'
#' For each historical window \eqn{CC_j}, the Euclidean distance is:
#' \deqn{dist(i,j) = ||CC_i - CC_j||}
#'
#' The forecast is computed as:
#' \deqn{\hat{C}_i = \frac{1}{\sum_{j=1}^{k} \alpha_j} \sum_{j=1}^{k} \alpha_j C_{q_j}}
#'
#' where \eqn{\alpha_j = 1 / dist(CC_{q_j}, CC_i)^2}
#'
#' @section Usage:
#' ```
#' wnn <- WNN$new(horizon = 10, window = 50, k = 5)
#' forecast <- wnn$fit_predict(tsdata)
#' ```
#'
#' @section Hyperparameters:
#' \describe{
#'   \item{horizon}{Forecast horizon \code{h}: number of future values to predict.}
#'   \item{window}{Window size \code{w}: number of consecutive samples for pattern matching.}
#'   \item{k}{Number of nearest neighbors to use for weighted averaging.}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(horizon, window, k)}}{Creates a new WNN model with specified parameters.}
#'   \item{\code{fit(tsdata)}}{Fits the model to the time series (stores training data).}
#'   \item{\code{predict(pattern)}}{Generates forecast using fitted model.}
#'   \item{\code{fit_predict(tsdata)}}{Fits the model and returns the forecast in one step.}
#' }
#'
#' @references
#' Talavera-Llames, R.L., Pérez-Chacón, R., Martínez-Ballesteros, M., Troncoso, A.,
#' Martínez-Álvarez, F. (2016). A Nearest Neighbours-Based Algorithm for Big Time
#' Series Data Forecasting. In: Hybrid Artificial Intelligent Systems. HAIS 2016.
#' Lecture Notes in Computer Science, vol 9648. Springer, Cham.
#'
#' @export
WNN <- R6::R6Class(
  classname = "WNN",

  # -- Public --
  public = list(

    #' @description
    #' Create a new WNN model
    #'
    #' @param horizon Integer. The forecast horizon (number of values to predict).
    #' @param window Integer. The window size for pattern matching.
    #' @param k Integer. The number of nearest neighbors to consider.
    #' @return A new WNN object.
    #'
    #' @examples
    #' wnn <- WNN$new(horizon = 10, window = 50, k = 5)
    initialize = function(horizon, window, k) {
      # Validate input parameters
      private$._validate_params(horizon, window, k)

      # Store hyperparameters
      private$._h <- horizon
      private$._w <- window
      private$._k <- k
    },

    #' @description
    #' Fit the model with training data
    #'
    #' Stores the time series data for later prediction.
    #'
    #' @param tsdata A univariate time series (ts object) or numeric vector.
    #' @return Invisible self (for method chaining).
    #'
    #' @examples
    #' data <- ts(rnorm(200), frequency = 12)
    #' wnn <- WNN$new(horizon = 12, window = 24, k = 3)
    #' wnn$fit(data)
    fit = function(tsdata) {
      # Validate input time series
      private$._validate_tsdata(tsdata)

      # Store the training data
      private$._original_data <- tsdata

      # Reset forecast results
      private$._forecast <- NULL
      private$._distances <- NULL
      private$._weights <- NULL
      private$._nearest_neighbors <- NULL

      invisible(self)
    },

    #' @description
    #' Generate predictions using the fitted model
    #'
    #' This method implements the WNN algorithm:
    #' 1. Extracts the reference window (last w observations from fitted data or custom pattern)
    #' 2. Finds the k nearest neighbors in the historical data
    #' 3. Computes a weighted average of their subsequent h values
    #'
    #' @param pattern Optional. A numeric vector of length w to use as reference pattern.
    #'   If NULL, uses the last w values of the fitted data.
    #' @return A time series object containing the h predicted values.
    #'
    #' @examples
    #' data <- ts(rnorm(200), frequency = 12)
    #' wnn <- WNN$new(horizon = 12, window = 24, k = 3)
    #' wnn$fit(data)
    #' forecast <- wnn$predict()
    predict = function(pattern = NULL) {
      # Check if model is fitted
      if (is.null(private$._original_data)) {
        stop("Model must be fitted before prediction. Call fit() first.")
      }

      tsdata <- private$._original_data
      n <- length(tsdata)

      # Extract the reference window CCi
      if (is.null(pattern)) {
        CCi <- tsdata[(n - private$._w + 1):n]
      } else {
        if (length(pattern) != private$._w) {
          stop(paste("Pattern must have length w =", private$._w))
        }
        CCi <- pattern
      }

      # Initialize storage for k nearest neighbors
      top_k_distances <- rep(Inf, private$._k)
      top_k_futures <- vector("list", private$._k)
      top_k_indices <- rep(NA, private$._k)

      # Maximum starting index for sliding window
      max_start <- n - private$._w - private$._h + 1

      # Slide through all possible windows
      for (i in seq_len(max_start)) {
        # window + h future values
        window_and_future <- tsdata[i:(i + private$._w + private$._h - 1)]

        # split into CCj and future values
        CCj <- window_and_future[1:private$._w]
        future_values <- window_and_future[(private$._w + 1):(private$._w + private$._h)]

        # Compute Euclidean distance
        dist_ij <- private$._euclidean_distance(CCi, CCj)

        # Check if this is among the k nearest
        max_dist_idx <- which.max(top_k_distances)
        if (dist_ij < top_k_distances[max_dist_idx]) {
          top_k_distances[max_dist_idx] <- dist_ij
          top_k_futures[[max_dist_idx]] <- future_values
          top_k_indices[max_dist_idx] <- i
        }
      }

      # Sort neighbors by distance
      sort_order <- order(top_k_distances)
      top_k_distances <- top_k_distances[sort_order]
      top_k_futures <- top_k_futures[sort_order]
      top_k_indices <- top_k_indices[sort_order]

      # Compute weights: alpha_j = 1 / (distance^2)
      epsilon <- 1e-10
      alpha <- 1 / ((top_k_distances + epsilon)^2)
      alpha <- alpha / sum(alpha)

      # Weighted forecast
      forecast <- rep(0, private$._h)
      for (j in seq_len(private$._k)) {
        forecast <- forecast + alpha[j] * top_k_futures[[j]]
      }

      # Convert to time series object if input was ts
      if (is.ts(tsdata)) {
        forecast <- private$._create_forecast_ts(forecast, tsdata)
      }

      # Store results
      private$._forecast <- forecast
      private$._distances <- top_k_distances
      private$._weights <- alpha
      private$._nearest_neighbors <- list(
        indices = top_k_indices,
        distances = top_k_distances,
        weights = alpha,
        future_values = top_k_futures
      )

      return(forecast)
    },

    #' @description
    #' Fit the model and generate predictions
    #'
    #' This method combines fit() and predict() in one step.
    #'
    #' @param tsdata A univariate time series (ts object) or numeric vector.
    #' @return A time series object containing the h predicted values.
    #'
    #' @examples
    #' data <- ts(rnorm(200), frequency = 12)
    #' wnn <- WNN$new(horizon = 12, window = 24, k = 3)
    #' forecast <- wnn$fit_predict(data)
    fit_predict = function(tsdata) {
      self$fit(tsdata)
      return(self$predict())
    },

    #' @description
    #' Print method for WNN object (brief summary)
    #' @return Invisible self
    print = function() {
      cat("WNN Model\n")
      cat("---------\n")
      cat("Horizon (h):", private$._h, "\n")
      cat("Window (w): ", private$._w, "\n")
      cat("Neighbors (k):", private$._k, "\n")
      cat("Status:", ifelse(is.null(private$._forecast), "Not fitted", "Fitted"), "\n")
      invisible(self)
    },

    #' @description
    #' Summary method for WNN object (complete information)
    #'
    #' Displays all information about the model including:
    #' - Hyperparameters (horizon, window, k)
    #' - Original data statistics (if fitted)
    #' - Nearest neighbor distances and weights
    #' - Forecast values
    #'
    #' @return Invisible self
    summary = function() {
      cat("=======================================================\n")
      cat("           WNN MODEL - COMPLETE SUMMARY\n")
      cat("=======================================================\n\n")

      # Hyperparameters
      cat(">> HYPERPARAMETERS\n")
      cat("   Horizon (h)     :", private$._h, "values to predict\n")
      cat("   Window (w)      :", private$._w, "values for pattern matching\n")
      cat("   Neighbors (k)   :", private$._k, "nearest neighbors\n\n")

      # Check if model is fitted
      if (is.null(private$._forecast)) {
        cat(">> STATUS: Model not fitted yet.\n")
        cat("   Use fit_predict(tsdata) to train the model.\n")
        cat("=======================================================\n")
        return(invisible(self))
      }

      cat(">> STATUS: Model fitted\n\n")

      # Original data statistics
      cat(">> ORIGINAL DATA\n")
      cat("   Length          :", length(private$._original_data), "observations\n")
      cat("   Min             :", round(min(private$._original_data), 4), "\n")
      cat("   Max             :", round(max(private$._original_data), 4), "\n")
      cat("   Mean            :", round(mean(private$._original_data), 4), "\n")
      cat("   Std Dev         :", round(sd(private$._original_data), 4), "\n")
      if (is.ts(private$._original_data)) {
        cat("   Frequency       :", frequency(private$._original_data), "\n")
        cat("   Start           :", paste(start(private$._original_data), collapse = "-"), "\n")
        cat("   End             :", paste(end(private$._original_data), collapse = "-"), "\n")
      }
      cat("\n")

      # Nearest neighbors info
      cat(">> NEAREST NEIGHBORS\n")
      cat("   Indices         :", paste(private$._nearest_neighbors$indices, collapse = ", "), "\n")
      cat("   Distances       :", paste(round(private$._distances, 4), collapse = ", "), "\n")
      cat("   Weights (alpha) :", paste(round(private$._weights, 4), collapse = ", "), "\n\n")

      # Forecast
      cat(">> FORECAST (", private$._h, " values)\n", sep = "")
      if (is.ts(private$._forecast)) {
        cat("   Start           :", paste(start(private$._forecast), collapse = "-"), "\n")
        cat("   Frequency       :", frequency(private$._forecast), "\n")
      }
      cat("   Values          :\n")
      print(round(private$._forecast, 4))
      cat("\n=======================================================\n")
      invisible(self)
    },

    #' @description Get the forecast horizon
    get_horizon = function() private$._h,

    #' @description Get the window size
    get_window = function() private$._w,

    #' @description Get the number of neighbors
    get_k = function() private$._k,

    #' @description Get the original training data
    get_original_data = function() private$._original_data,

    #' @description Get the forecast results
    get_forecast = function() private$._forecast,

    #' @description Get the distances of the k nearest neighbors
    get_distances = function() private$._distances,

    #' @description Get the weights used for the weighted average
    get_weights = function() private$._weights,

    #' @description Get the k nearest neighbors information
    get_nearest_neighbors = function() private$._nearest_neighbors
  ),

  # -- Private --
  private = list(
    # Hyperparameters
    ._h = NULL,  # horizon
    ._w = NULL,  # window size
    ._k = NULL,  # number of neighbors

    # Stored results
    ._original_data = NULL,
    ._forecast = NULL,
    ._distances = NULL,
    ._weights = NULL,
    ._nearest_neighbors = NULL,

    # Euclidean distance between two vectors
    ._euclidean_distance = function(vec1, vec2) {
      sqrt(sum((vec1 - vec2)^2))
    },

    # Validate constructor parameters (must be positive integers)
    ._validate_params = function(h, w, k) {
      # Helper function to check positive integer
      check_positive_integer <- function(x, name) {
        if (missing(x) || is.null(x)) stop(paste(name, "is required"))
        if (!is.numeric(x) || length(x) != 1) stop(paste(name, "must be a single numeric value"))
        if (x != as.integer(x)) stop(paste(name, "must be an integer"))
        if (x <= 0) stop(paste(name, "must be a positive integer"))
      }
      check_positive_integer(h, "horizon")
      check_positive_integer(w, "window")
      check_positive_integer(k, "k")
    },

    # Validate time series input
    ._validate_tsdata = function(tsdata) {
      if (missing(tsdata) || is.null(tsdata)) stop("tsdata is required")
      if (!is.numeric(tsdata)) stop("tsdata must be numeric (ts object or numeric vector)")

      n <- length(tsdata)
      min_length <- private$._w + private$._h

      if (n < min_length) {
        stop(paste0("Time series too short. Need at least ", min_length, " observations, got ", n))
      }

      # Check for k neighbors requirement
      max_neighbors <- n - private$._w - private$._h + 1
      if (max_neighbors < private$._k) {
        stop(paste0("Not enough data for ", private$._k, " neighbors. Maximum: ", max_neighbors))
      }
    },


    # Create forecast as time series object
    #
    # Converts the numeric forecast vector into a ts object that continues
    # from where the original time series ended.
    #
    # Example: If original_ts ends at (1960, 12) with frequency 12,
    #          the forecast will start at (1961, 1).
    ._create_forecast_ts = function(forecast_values, original_ts) {
      freq <- frequency(original_ts)
      end_time <- end(original_ts)
      start_time <- end_time + c(0, 1)

      if (start_time[2] > freq) {
        start_time <- c(start_time[1] + 1, start_time[2] - freq)
      }

      ts(forecast_values, start = start_time, frequency = freq)
    }
  )
)
