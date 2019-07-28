#' changeParameter
#' @description change analysis parameters
#' @param parameterName Name of the parameter to change
#' @param newValue New value of the parameter
#' @param parameters S4 object of class AnalysisParameters in which to change parameters
#' @details
#' For the parameter name selected, all parameters with that name will be altered.
#' To individually change identically named parameters use the \code{@} operator to access the appropriate slot directly.
#' @examples 
#' p <- analysisParameters()
#' p <- changeParameter('clusterType','PSOCK',p)
#' @export

changeParameter <- function(parameterName,newValue,parameters) {
  
  elements <- slotNames(parameters)
  elements <- elements[sapply(elements,function(x,parameters){
    length(slot(parameters,x))
  },parameters = parameters) > 0]
  
  if ('preTreat' %in% elements) {
    pars <- lapply(parameters@preTreat,function(x,parameterName){
      x <- lapply(x,function(y,parameterName){names(y)[names(y) == parameterName]},parameterName = parameterName)
      x[sapply(x,length) == 0] <- NULL
      return(x)
    },parameterName = parameterName)
    pars[sapply(pars,length) == 0] <- NULL
    pars <- lapply(names(pars),function(x,pars){
      pars <- pars[[x]]
      pars <- lapply(names(pars),function(y,pars,n){
        pars <- pars[[y]]
        pars <- c(n,y,pars)
        return(pars)
      },pars = pars,n = x)
      return(pars)
    },pars = pars)
    pars <- unlist(pars,recursive = F)
    
    for (i in 1:length(pars)) {
      parameters@preTreat[[pars[[i]][1]]][[pars[[i]][2]]][[pars[[i]][3]]] <- newValue
    }
  }
  
  if ('modelling' %in% elements) {
    pars <- parameters@modelling %>%
      map(~{
        p <- .
        if (parameterName %in% names(p)) {
          p[[parameterName]] <- newValue
        }
        return(p)
      })
    parameters@modelling <- pars
  }
  
  if ('correlations' %in% elements) {
    pars <- names(parameters@correlations)
    if (parameterName %in% pars) {
      parameters@correlations[[parameterName]] <- newValue
    }
  }
  
  return(parameters)
}