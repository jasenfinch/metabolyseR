#' correlationResults-Analysis
#' @rdname correlationResults
#' @description Extract correlation results from an Analysis object
#' @param x Analysis object
#' @export

setMethod('correlationResults', signature = 'Analysis',
          function(x){
            x <- x@correlations
            return(x)
          }
)