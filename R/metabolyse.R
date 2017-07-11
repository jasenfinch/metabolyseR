#' metabolyse 
#' @description Analyse data based on specified analysis elements.
#' @param  data tibble or data.frame containing data to analyse
#' @param info tibble or data.frame containing data info or meta data
#' @param parameters an object of AnalysisParameters class containing parameters for analysis. Default calls \code{analysisParameters()}
#' @importFrom magrittr %>%
#' @importFrom methods slotNames slot
#' @importFrom tibble as_tibble
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
      rawData = list(Info = as_tibble(info),Data = as_tibble(data)),
      preTreated = list(),
      classification = as_tibble(data.frame()),
      featureSelection = as_tibble(data.frame()),
      correlations = as_tibble(data.frame())
  )
  
  elements <- slotNames(analysis@parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = analysis@parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    analysis <- analysis %>% method() 
  }
  return(analysis)
}