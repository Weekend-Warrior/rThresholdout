#' @title Thresholdout
#'
#' @description
#' Create Thresholdout object for adaptive data analysis and holdout reuse.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new(threshold, sigma, budget)}}{Creates a new \code{Thresholdout} object.}
#'   \item{\code{$query(train_val, holdout_val)}}{Sends a query to the holdout dataset.}
#' }
#'
#' @section Actives:
#' \describe{
#'   \item{\code{$threshold}}{gets the threshold value.}
#'   \item{\code{$sigma}}{gets the sigma value.}
#'   \item{\code{$budget}}{gets the remaining budget value.}
#'   \item{\code{$record}}{gets the record of previous queries.}
#' }
#'
#' @examples
#' library(rThresholdout)
#' Thresholdout_Obj <- Thresholdout$new(threshold = 0.04, sigma = 0.01, budget = 1000)
#' Thresholdout_Obj$query(train_val = c(0.06, 0.07), holdout_val = c(0.05, 0.08))
#'
#' @importFrom stats runif
#' @importFrom R6 R6Class
#' @importFrom assertive.types assert_is_numeric
#' @export

Thresholdout <- R6Class(
  classname = "Thresholdout",
  public = list(
    initialize = function(threshold, sigma, budget) {
      private$..threshold <- threshold
      private$..sigma <- sigma
      private$..budget <- budget
    },
    query = function(train_val, holdout_val) {
      if (length(train_val) != length(holdout_val)) {
        stop("The inputs of training and holdout should be compatible")
      } else {
        N <- length(train_val)
      }
      if (private$..budget < 1) {
        message("Budget exhausted")
      } else {
        private$..noise_update(n = N)
        threshold_ind <- abs(train_val - holdout_val) > private$..threshold + private$..gamma + private$..eta
        if (private$..budget - sum(threshold_ind) < 0) {
          message("Budget is not enough for current query")
        }
        private$..budget <- private$..budget - sum(threshold_ind)
        result <- holdout_val + private$..xi
        result[!threshold_ind] <- train_val[!threshold_ind]
        private$..record <- append(private$..record, list(data.table::data.table(matrix(result, nrow = 1))))
        return(result)
      }
    }
  ),
  private = list(
    ..threshold = NULL,
    ..sigma = NULL,
    ..budget = NULL,
    ..xi = NULL,
    ..gamma = NULL,
    ..eta = NULL,
    ..record = list(),
    ..noise_update = function(n) {
      private$..xi <- rLaplace(n = n, s = 1 * private$..sigma)
      private$..gamma <- rLaplace(n = n, s = 2 * private$..sigma)
      private$..eta <- rLaplace(n = n, s = 4 * private$..sigma)
    }
  ),
  active = list(
    threshold = function(value) {
      private$..threshold
    },
    sigma = function(value) {
      private$..sigma
    },
    budget = function() {
      private$..budget
    },
    record = function() {
      data.table::rbindlist(private$..record)
    }
  ),
  inherit = NULL,
  lock_objects = TRUE,
  class = TRUE,
  portable = TRUE,
  lock_class = TRUE,
  cloneable = FALSE
)

rLaplace <- function (n = 1, m = 0, s = 1) {
  q <- runif(n)
  ifelse(q < 0.5, s * log(2 * q) + m, -s * log(2 * (1 - q)) + m)
}

