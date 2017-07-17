#' featureSelectionResults-Analysis
#' @rdname featureSelectionResults
#' @description Extract feature selection results from an Analysis object
#' @param x Analysis object
#' @export

setMethod('featureSelectionResults', signature = 'Analysis',
          function(x){
            x <- x@featureSelection
            return(x)
          }
)