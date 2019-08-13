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

#' raw
#' @rdname raw
#' @description Get or set an AnalysisData object from the rawData slot of the Analysis class.
#' @param x S4 object of class Analysis
#' @param value S4 object of class AnalysisData 
#' @export 

setMethod('raw',signature = 'Analysis',
          function(x){
            x@rawData
          }
)

#' @rdname raw
#' @export

`raw<-` <- function(x,value){
  x@rawData <- value
  return(x)
}