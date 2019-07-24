#' rawInfo-Analysis
#' @rdname rawInfo
#' @description Extract raw info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('rawInfo', signature = 'Analysis',
          function(x){
            x <- x@rawData %>%
              info()
            return(x)
          }
)