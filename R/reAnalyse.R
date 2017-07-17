#' reAnalyse
#' @description Re-analyse an object of class Analysis using specified parameters.
#' @importFrom methods slot slot<-
#' @param analysis an object of class Analysis containing previous analysis results
#' @param parameters an object of class Parameters containing parameters for re-analysis
#' @seealso \link{metabolyse} \link{analysisParameters} \linkS4class{AnalysisParameters} \linkS4class{Analysis}
#' @export

reAnalyse <- function(analysis,parameters = analysisParameters()){
  elements <- slotNames(parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    slot(analysis@parameters,i) <- slot(parameters,i)
    analysis <- analysis %>% method() 
  }
  return(analysis)
}
