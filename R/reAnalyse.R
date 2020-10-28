#' reAnalyse
#' @description Re-analyse an object of class Analysis using 
#' specified parameters.
#' @importFrom methods slot slot<-
#' @param analysis an object of class Analysis containing previous 
#' analysis results
#' @param parameters an object of class Parameters containing parameters for 
#' re-analysis
#' @param verbose should output be printed to the console 
#' @examples
#' library(metaboData)
#' 
#' ## Generate analysis parameters
#' p <- analysisParameters(c('pre-treatment','modelling'))
#' 
#' ## Alter pre-treatment and modelling parameters to use different methods
#' parameters(p,'pre-treatment') <- preTreatmentParameters(
#'   list(occupancyFilter = 'maximum',
#'        transform = 'TICnorm')
#' )
#' parameters(p,'modelling') <- modellingParameters('anova')
#' 
#' ## Change "cls" and "nCores" parameters 
#' changeParameter(p,'cls') <- 'day'
#' changeParameter(p,'nCores') <- 2
#' 
#' ## Run analysis using a subset of the abr1 negative mode data set
#' analysis <- metabolyse(abr1$neg[,1:200],
#'                        abr1$fact,
#'                        p)
#'                        
#' ## Re-analyse to include correlation analysis
#' analysis <- reAnalyse(analysis,
#'                       parameters = analysisParameters('correlations'))
#' @export

reAnalyse <- function(analysis,
                      parameters = analysisParameters(), 
                      verbose = TRUE){
  version <- packageVersion('metabolyseR') %>% as.character()
  analysisStart <- date()
  if (verbose == TRUE) {
    startTime <- proc.time()
    cat('\n',
        blue('metabolyseR'),
        ' ',
        red(str_c('v',version)),
        ' ',
        analysisStart,
        '\n',
        sep = '')
    cat(rep('_',console_width()),'\n',sep = '')
    print(parameters)
    cat(rep('_',console_width()),'\n\n',sep = '')
  }
  
  elements <- slotNames(parameters)
  elements <- elements[sapply(
    elements,
    function(x,
             parameters){length(slot(parameters,x))},
    parameters = parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    slot(analysis@parameters,i) <- slot(parameters,i)
    analysis <- analysis %>% method() 
  }
  
  if (verbose == TRUE) {
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
