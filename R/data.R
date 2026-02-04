#' AirPassengers Dataset (Built-in R Dataset)
#'
#' Monthly totals of international airline passengers from 1949 to 1960.
#' This is a classic time series dataset included in base R that can be used
#' to demonstrate the WNN forecasting algorithm.
#'
#' @format A time series object (ts) with 144 monthly observations from 1949 to 1960.
#'
#' @source Box, G. E. P., Jenkins, G. M. and Reinsel, G. C. (1976)
#' Time Series Analysis, Forecasting and Control. Third Edition. Holden-Day.
#'
#' @examples
#' # Load the built-in dataset
#' data(AirPassengers)
#' plot(AirPassengers)
#'
#' # Use with WNN model
#' wnn_model <- WNN$new(horizon = 12, window = 24, k = 3)
#' forecast <- wnn_model$fit_predict(AirPassengers)
#' print(forecast)
#'
#' @name AirPassengers
#' @docType data
NULL
