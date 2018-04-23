#' rawInfo-Analysis
#' @rdname rawInfo
#' @description Extract raw info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('rawData', signature = 'Analysis',
          function(x){
            x <- x@rawData$Info
            return(x)
          }
)