#' preTreatedInfo-Analysis
#' @rdname preTreatedInfo
#' @description Extract pre-treated info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('preTreatedInfo', signature = 'Analysis',
          function(x){
            x <- x@preTreated$Info
            return(x)
          }
)