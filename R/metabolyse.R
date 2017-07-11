#' metabolyse 
#' @description Analyse data based on specified analysis elements.
#' @param  data tibble or data.frame containing data to analyse
#' @param info tibble or data.frame containing data info or meta data
#' @param parameters an object of AnalysisParameters class containing parameters for analysis. Default calls \code{analysisParameters()}
#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df
#' @importFrom methods slotNames slot
#' @seealso \linkS4class{AnalysisParameters} \link{analysisParameters}
#' @examples 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters('preTreat')
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)  
#' @export

metabolyse <- function(data,info,parameters = analysisParameters()){
  analysis <- new('Analysis',
      log = list(analysis = date()),
      parameters = parameters,
      rawData = list(Info = info,Data = data),
      preTreated = list(),
      classification = tbl_df(data.frame()),
      featureSelection = tbl_df(data.frame()),
      correlations = tbl_df(data.frame())
  )
  
  elements <- slotNames(analysis@parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = analysis@parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    analysis <- analysis %>% method() 
  }
  return(analysis)
}