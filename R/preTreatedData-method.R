#' preTreatedData-Analysis
#' @rdname preTreatedData
#' @description Extract pre-treated data from an Analysis object
#' @param x Analysis object
#' @export

setMethod('preTreatedData', signature = 'Analysis',
          function(x){
            x <- x@preTreated %>%
              dat()
            return(x)
          }
)

#' preTreated
#' @rdname preTreated
#' @description Get or set an AnalysisData object from the preTreated slot of the Analysis class.
#' @param x S4 object of class Analysis
#' @param value S4 object of class AnalysisData 
#' @export 

setMethod('preTreated',signature = 'Analysis',
          function(x){
            x@preTreated
          }
)

#' @rdname preTreated
#' @export

`preTreated<-` <- function(x,value){
  x@preTreated <- value
  return(x)
}