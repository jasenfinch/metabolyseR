#' analysisData
#' @description Create an S4 Data object.
#' @param data sample data
#' @param info saple info
#' @export

analysisData <- function(data,info){
  if (nrow(data) != nrow(info)) {
    stop('Number of rows in data should match number of rows in info!')
  }
  
  d <- new('Data')
  d@data <- data %>%
    as_tibble()
  d@info <- info %>%
    as_tibble()
  return(d)
}

#' dat
#' @rdname dat
#' @description Return sample data from a Data object.
#' @param x S4 object of class Data 
#' @export

setMethod('dat',signature = 'Data',
          function(x){
            x@data
          })

#' info
#' @rdname info
#' @description Return sample info from a Data object.
#' @param x S4 object of class Data 
#' @export

setMethod('info',signature = 'Data',
          function(x){
            x@info
          })