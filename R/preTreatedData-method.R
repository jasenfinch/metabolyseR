#' preTreatedData-Analysis
#' @rdname preTreatedData
#' @description Extract pre-treated data from an Analysis object
#' @param x Analysis object
#' @export

setMethod('preTreatedData', signature = 'Analysis',
          function(x){
            x <- x@preTreated$Data
            return(x)
          }
)