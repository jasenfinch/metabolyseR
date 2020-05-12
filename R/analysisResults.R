#' analysisResults
#' @rdname analysisResults
#' @description Extract analysis results for a given analysis element.
#' @param x S4 object of class Analysis
#' @param element Analysis element to extract. Should be one of those returned \code{analysisElements()}.
#' @export

setMethod('analysisResults',signature = 'Analysis',
          function(x,element){
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(str_c('Argument "element" should be one of ',str_c(elements,collapse = ', '),'.'),call. = FALSE)
            }
            
            if (element == 'pre-treatment') {
              d <- preTreated(x)
            } else {
              d <- slot(x,element)
            }
            
            return(d)
          }
)

#' preTreatedInfo-Analysis
#' @rdname preTreatedInfo
#' @description Extract pre-treated info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('preTreatedInfo', signature = 'Analysis',
          function(x){
            x <- x@`pre-treated` %>%
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
            x <- x@`pre-treated` %>%
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
            x@`pre-treated`
          }
)

#' @rdname preTreated
#' @export

`preTreated<-` <- function(x,value){
  x@`pre-treated` <- value
  return(x)
}

#' rawInfo-Analysis
#' @rdname rawInfo
#' @description Extract raw info from an Analysis object
#' @param x Analysis object
#' @export

setMethod('rawInfo', signature = 'Analysis',
          function(x){
            x <- x@raw %>%
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
            x <- x@raw %>%
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
            x@raw
          }
)

#' @rdname raw
#' @export

`raw<-` <- function(x,value){
  x@raw <- value
  return(x)
}