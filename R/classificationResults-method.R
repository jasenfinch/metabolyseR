#' classificationResults-Analysis
#' @rdname classificationResults
#' @description Extract classification results from an Analysis object
#' @param x Analysis object
#' @export

setMethod('classificationResults', signature = 'Analysis',
          function(x){
            x <- x@classification
            return(x)
          }
)