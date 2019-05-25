#' analysisData
#' @description Create an S4 AnalysisData object.
#' @param data sample data
#' @param info saple info
#' @export

analysisData <- function(data,info){
  if (nrow(data) != nrow(info)) {
    stop('Number of rows in data should match number of rows in info!')
  }
  
  d <- new('AnalysisData')
  d@data <- data %>%
    as_tibble()
  d@info <- info %>%
    as_tibble()
  return(d)
}

#' dat
#' @rdname dat
#' @description Return sample data from an AnalysisData object.
#' @param x S4 object of class Data 
#' @export

setMethod('dat',signature = 'AnalysisData',
          function(x){
            x@data
          })

#' info
#' @rdname info
#' @description Return sample info from an AnalysisData object.
#' @param x S4 object of class Data 
#' @export

setMethod('info',signature = 'AnalysisData',
          function(x){
            x@info
          })