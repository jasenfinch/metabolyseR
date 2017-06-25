#' metabolyse 
#' @importFrom magrittr %>%
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

metabolyse <- function(data,info,params = analysisParameters()){
  analysis <- new('Analysis',
      log = list(analysis = date()),
      parameters = params,
      rawData = list(Info = info,Data = data),
      preTreated = list(),
      classification = list(),
      featureSelection = list()
  )
  
  elements <- slotNames(analysis@parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = analysis@parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    analysis <- analysis %>% method() 
  }
  return(analysis)
}