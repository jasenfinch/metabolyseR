#' Random forest
#' @rdname randomForest
#' @description Perform random forest on an `AnalysisData` object
#' @param x S4 object of class `AnalysisData`
#' @param cls vector of sample information columns to use for response variable information. Set to NULL for unsupervised.
#' @param rf named list of arguments to pass to `randomForest::randomForest`
#' @param reps number of repetitions to perform
#' @param binary TRUE/FALSE should binary comparisons be performed. Ignored for unsupervised and regression. Ignored if `comparisons` specified.
#' @param comparisons list of comparisons to perform. 
#' Ignored for unsupervised and regression. See details. 
#' @param perm number of permutations to perform. Ignored for unsupervised.
#' @param returnModels TRUE/FALSE should model objects be returned.
#' @param seed random number seed
#' @return An S4 object of class `RandomForest`.
#' @details Specified class comparisons should be given as a list named 
#' according to \code{cls}. Comparisons should be given as class names 
#' separated by '~' (eg. '1~2~H').
#' @examples 
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' plotMDS(rf,cls = 'day')
#' @export

setGeneric("randomForest", 
           function(
             x, 
             cls = 'class',
             rf = list(), 
             reps = 1, 
             binary = FALSE, 
             comparisons = list(), 
             perm = 0, 
             returnModels = FALSE, 
             seed = 1234)
             standardGeneric("randomForest"))

#' @rdname randomForest

setMethod('randomForest',signature = 'AnalysisData',
          function(x, 
                   cls = 'class', 
                   rf = list(), 
                   reps = 1, 
                   binary = FALSE, 
                   comparisons = list(), 
                   perm = 0, 
                   returnModels = FALSE, 
                   seed = 1234){
            
            rf$keep.forest <- TRUE
            rf$proximity <- TRUE
            rf$importance <- TRUE
            
            if (is.null(cls)) {
              res <- unsupervised(x,
                                  rf,
                                  reps,
                                  returnModels,
                                  seed)
            } else {
              res <- supervised(x,
                                cls,
                                rf,
                                reps,
                                binary,
                                comparisons,
                                perm,
                                returnModels,
                                seed)
            }
            
            if (is.list(res) & {length(res) == 1}){
              res <- res[[1]]
            }
            
            return(res)
            
          }
)
