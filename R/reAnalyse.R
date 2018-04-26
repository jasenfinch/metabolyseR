#' reAnalyse
#' @description Re-analyse an object of class Analysis using specified parameters.
#' @importFrom methods slot slot<-
#' @param analysis an object of class Analysis containing previous analysis results
#' @param parameters an object of class Parameters containing parameters for re-analysis
#' @param verbose should output be printed to the console 
#' @seealso \link{metabolyse} \link{analysisParameters} \linkS4class{AnalysisParameters} \linkS4class{Analysis}
#' @export

reAnalyse <- function(analysis,parameters = analysisParameters(), verbose = T){
  version <- packageVersion('metabolyseR')
  analysisStart <- date()
  if (verbose == T) {
    startTime <- proc.time()
    cat('\n',blue('metabolyseR'),' ',red(str_c('v',version)),' ',analysisStart,'\n',sep = '')
    cat(rep('_',console_width()),'\n',sep = '')
    print(parameters)
    cat(rep('_',console_width()),'\n\n',sep = '')
  }
  
  elements <- slotNames(parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    slot(analysis@parameters,i) <- slot(parameters,i)
    analysis <- analysis %>% method() 
  }
  
  if (verbose == T) {
    endTime <- proc.time()
    elapsed <- {endTime - startTime} %>%
      .[3] %>%
      round(1) %>%
      seconds_to_period() %>%
      str_c('[',.,']')
    
    cat(rep('_',console_width()),'\n',sep = '')
    cat('\n',green('Complete! '),elapsed,'\n\n',sep = '')
  }
  return(analysis)
}
