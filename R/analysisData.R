#' AnalysisData class constructor
#' @description Create an AnalysisData S4 object.
#' @param data table containing sample metabolomic data
#' @param info table containing sample meta information
#' @return An S4 object of class Analysis.
#' @examples 
#' library(metaboData)
#' d <- analysisData(data = abr1$neg,info = abr1$fact)
#' 
#' print(d)
#' @export

analysisData <- function(data,info){
  
  d <- new('AnalysisData',
           data = as_tibble(data),
           info = as_tibble(info))
  
  return(d)
}
