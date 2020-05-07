#' changeParameter
#' @rdname changeParameter
#' @description change analysis parameters
#' @param parameterName Name of the parameter to change
#' @param newValue New value of the parameter
#' @param parameters S4 object of class AnalysisParameters in which to change parameters
#' @param elements Character vector of analysis elements to target parameter change. Can be any returned by \code{analysisElements}.
#' @details
#' For the parameter name selected, all parameters with that name will be altered.
#' To individually change identically named parameters use the \code{@} operator to access the appropriate slot directly.
#' @examples 
#' p <- analysisParameters()
#' p <- changeParameter(p,'clusterType','PSOCK')
#' @importFrom purrr map_lgl
#' @export

setMethod('changeParameter',signature = 'AnalysisParameters',
          function(x,parameterName,newValue,elements = analysisElements()) {
  
  ele <- analysisElements()
  
  if (F %in% (map_lgl(elements,~{. %in% ele}))) {
    e <- str_c('"',ele,'"')
    stop(str_c('Elements can only include ',str_c(e,collapse = ', ')))
  }
  
  elements <- elements[sapply(elements,function(x,parameters){
    length(slot(parameters,x))
  },parameters = x) > 0]
  
  if ('pre-treatment' %in% elements) {
    pars <- lapply(parameters(x,'pre-treatment'),function(x,parameterName){
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
    
    if (!is.null(pars)) {
      p <- parameters(x,'pre-treatment')
      for (i in 1:length(pars)) {
        p[[pars[[i]][1]]][[pars[[i]][2]]][[pars[[i]][3]]] <- newValue
      }
      parameters(x,'pre-treatment') <- p
    }
  }
  
  if ('modelling' %in% elements) {
    pars <- parameters(x,'modelling') %>%
      map(~{
        p <- .
        if (parameterName %in% names(p)) {
          p[[parameterName]] <- newValue
        }
        return(p)
      })
    parameters(x,'modelling') <- pars
  }
  
  if ('correlations' %in% elements) {
    pars <- names(parameters(x,'correlations'))
    if (parameterName %in% pars) {
      parameters(x,'correlations')[[parameterName]] <- newValue
    }
  }
  
  return(x)
})