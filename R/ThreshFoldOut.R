# In simple terms: given a model structure and a metric, perform k-fold cross validation estimating tolerance and threshold

#' @export

ThreshFoldOut <- R6Class('ThreshFoldOut',
                         inherit = HoldoutMetrics,
                         public = list(),
                         private = list(),
                         active = list())