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
#' model <- glm(y ~ ., data = train, family="binomial")
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
#' Thresholdout_Obj <- Thresholdout$new(Threshold = 0.04, Sigma = 0.01, Budget = 1000)
#' Thresholdout_Obj$query(train_val = train_metrics, holdout_val = holdout_metrics)
#' 
#' 
#' # HoldoutMetrics simplifies this process
#' HoldoutMetrics_Obj <- HoldoutMetrics$new(metric = auc,
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
#' @export

HoldoutMetrics <- R6Class('HoldoutMetrics',
                          public = list(initialize = function(metric,
                                                              train_target,
                                                              holdout_target,
                                                              threshold,
                                                              sigma,
                                                              budget) {
                                          private$metric <- metric
                                          private$train_target <- train_target
                                          private$holdout_target <- holdout_target
                                          private$thresholdout <- Thresholdout$new(Threshold = threshold, 
                                                                                   Sigma = sigma, 
                                                                                   Budget = budget)
                                        },
                                        query = function(train_pred,
                                                         holdout_pred){
                                          train_metric <- private$metric(actual = private$train_target,
                                                                         predicted = train_pred)[[1]] 
                                          
                                          holdout_metric <- private$metric(actual = private$holdout_target,
                                                                           predicted = holdout_pred)[[1]]
                                          
                                          return(private$thresholdout$query(train_val = train_metric,
                                                                            holdout_val = holdout_metric))
                                        }),
                          private = list(metric = NULL,
                                         train_target = NULL,
                                         holdout_target = NULL,
                                         thresholdout = NULL),
                          active = NULL,
                          inherit = NULL,
                          lock_objects = TRUE,
                          class = TRUE,
                          portable = TRUE,
                          lock_class = TRUE,
                          cloneable = FALSE)