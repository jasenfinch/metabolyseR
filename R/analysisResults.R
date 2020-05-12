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

#' modellingResults
#' @rdname modellingResults
#' @description Return modelling results from an Analysis object.
#' @param x S4 object of class Analysis
#' @export

setMethod('modellingResults',signature = 'Analysis',
          function(x){
            x@modelling
          }
)

#' preTreatedInfo-Analysis
#' @rdname preTreatedInfo
#' @description Extract pre-treated info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('preTreatedInfo', signature = 'Analysis',
          function(x){
            x <- x@preTreated %>%
              sinfo()
            return(x)
          }
)

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

#' rawInfo-Analysis
#' @rdname rawInfo
#' @description Extract raw info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('rawInfo', signature = 'Analysis',
          function(x){
            x <- x@rawData %>%
              sinfo()
            return(x)
          }
)

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