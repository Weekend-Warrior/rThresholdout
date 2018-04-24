#' @title HoldoutMetrics
#'
#' @description
#' Create HoldoutMetrics object
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{$new(metric, train_target, holdout_target, threshold, sigma, budget)}}{Creates a new \code{Thresholdout} object.}
#'   \item{\code{$query(train_pred, holdout_pred)}}{Sends a query to the holdout dataset.}
#' }
#'
#' @section Actives:
#' \describe{None. See Thresholdout.}
#'
#' @examples
#' library(rThresholdout)
#' library(ModelMetrics)
#' 
#' # split data
#' data(testDF)
#' train <- testDF[1:50,]
#' holdout <- testDF[51:100,]
#' 
#' # identify targets
#' train_target <- train$y
#' holdout_target <- holdout$y
#' 
#' # build a model
#' model <- glm(y ~ ., data = train, family = "binomial")
#' 
#' # generate predictions
#' train_preds <- predict(model, train)
#' holdout_preds <- predict(model, holdout)
#' 
#' # ModelMetrics provides a consistent Rcpp implementation for several measures of fit
#' train_metrics <- auc(actual = train_target, predicted = train_preds)
#' holdout_metrics <- auc(actual = holdout_target, predicted = holdout_preds)
#' 
#' # Thresholdout returns metrics derived from train and holdout data sets.
#' Thresholdout_Obj <- Thresholdout$new(threshold = 0.04, sigma = 0.01, budget = 1000)
#' Thresholdout_Obj$query(train_val = train_metrics, holdout_val = holdout_metrics)
#' 
#' 
#' # HoldoutMetrics simplifies this process
#' HoldoutMetrics_Obj <- HoldoutMetrics$new(metric = 'auc',
#'                                          train_target = train_target,
#'                                          holdout_target = holdout_target,
#'                                          threshold = 0.04,
#'                                          sigma = 0.01,
#'                                          budget = 1000)
#' 
#' HoldoutMetrics_Obj$query(train_preds,
#'                          holdout_preds)
#' 
#' @import ModelMetrics
#' @importFrom R6 R6Class
#' @importFrom assertive.types assert_is_numeric
#' @importFrom assertive.types assert_is_function
#' @importFrom assertive.types assert_is_character
#' @export

HoldoutMetrics <- R6Class('HoldoutMetrics',
                          inherit = Thresholdout,
                          public = list(initialize = function(metric,
                                                              train_target,
                                                              holdout_target,
                                                              threshold,
                                                              tolerance) {
                                          private$..metric <- if(is.character(metric)) {
                                            assertive.types::assert_is_character(metric)
                                            private$..metric <- eval(parse(text = paste0("ModelMetrics::", metric)))
                                          } else {
                                            assertive.types::assert_is_function(metric)
                                            private$..metric <- metric
                                          }
                                          private$..train_target <- train_target
                                          private$..holdout_target <- holdout_target
                                          private$..budget <- ceiling((tolerance)^2 * 
                                                                        length(holdout_target))
                                          private$..sigma <- 2 * tolerance
                                          private$..threshold <- threshold
                                        },
                                        query = function(train_pred,
                                                         holdout_pred){
                                          train_val <- private$..metric(actual = private$..train_target,
                                                                           predicted = train_pred)[[1]] 
                                          
                                          holdout_val <- private$..metric(actual = private$..holdout_target,
                                                                             predicted = holdout_pred)[[1]]
                                          
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
                                            if (private$..budget - sum(threshold_ind) < 1) {
                                              message("Budget is not enough for current query")
                                            }
                                            private$..budget <- private$..budget - sum(threshold_ind)
                                            result <- holdout_val + private$..xi
                                            result[!threshold_ind] <- train_val[!threshold_ind]
                                            private$..record <- append(private$..record, list(data.table::data.table(matrix(result, nrow = 1))))
                                            return(result)
                                          }
                                        }),
                          private = list(..metric = NULL,
                                         ..train_target = NULL,
                                         ..holdout_target = NULL),
                          active = list(metric = function(value) {
                                          if(!missing(value)) {
                                            if(is.character(value)) {
                                              assertive.types::assert_is_character(value)
                                              private$..metric <- eval(parse(text = paste0("ModelMetrics::", value)))
                                            } else {
                                              assertive.types::assert_is_function(value)
                                              private$..metric <- value
                                            }
                                          } else private$..metric
                                        },
                                        train_target = function(value) {
                                          if(!missing(value)) {
                                            private$..train_target <- value
                                          } else private$..train_target
                                        },
                                        holdout_target = function() {
                                          private$..holdout_target
                                        },
                                        threshold = function(value) {
                                          if(!missing(value)) {
                                            assert_is_numeric(value)
                                            private$..threshold <- value
                                          } else private$..threshold
                                        },
                                        tolerance = function(value) {
                                          if(!missing(value)) {
                                            assert_is_numeric(value)
                                            private$..budget <- ceiling(private$..budget * 
                                                                          (value / (private$..sigma / 2))^2)
                                            private$..sigma <- value
                                          } else private$..sigma / 2
                                        }))
