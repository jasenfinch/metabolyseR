#' reAnalyse
#' @export

reAnalyse <- function(analysis,params = analysisParameters()){
  elements <- slotNames(params)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = params) > 0]
  
  for (i in elements) {
    method <- get(i)
    slot(analysis@parameters,i) <- slot(params,i)
    analysis <- analysis %>% method() 
  }
  return(analysis)
}
