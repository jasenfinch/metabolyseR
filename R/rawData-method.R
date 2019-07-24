#' rawData-Analysis
#' @rdname rawData
#' @description Extract raw data from an Analysis object
#' @param x Analysis object
#' @export

setMethod('rawData', signature = 'Analysis',
          function(x){
            x <- x@rawData %>%
              dat()
            return(x)
          }
)