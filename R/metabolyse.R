#' metabolyse 
#' @description Analyse data based on specified analysis elements.
#' @param  data tibble or data.frame containing data to analyse
#' @param info tibble or data.frame containing data info or meta data
#' @param parameters an object of AnalysisParameters class containing parameters for analysis. Default calls \code{analysisParameters()}
#' @param verbose should output be printed to the console 
#' @importFrom magrittr %>%
#' @importFrom methods slotNames slot
#' @importFrom tibble tibble as_tibble 
#' @importFrom utils packageVersion
#' @importFrom cli console_width
#' @importFrom crayon yellow green
#' @importFrom lubridate seconds_to_period
#' @seealso \linkS4class{AnalysisParameters} \link{analysisParameters}
#' @examples 
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters('preTreat')
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)  
#' @export

metabolyse <- function(data,info,parameters = analysisParameters(), verbose = T){
  version <- packageVersion('metabolyseR')
  analysisStart <- date()
  
  if (verbose == T) {
    startTime <- proc.time()
    cat('\n',blue('metabolyseR'),' ',red(str_c('v',version)),' ',analysisStart,'\n',sep = '')
    cat(rep('_',console_width()),'\n',sep = '')
    print(parameters)
    cat(rep('_',console_width()),'\n\n',sep = '')
  }
  
  analysis <- new('Analysis',
      log = list(packageVersion = version,analysis = analysisStart,verbose = verbose),
      parameters = parameters,
      rawData = list(Info = as_tibble(info),Data = as_tibble(data)),
      preTreated = list(),
      classification = tibble(),
      featureSelection = tibble(),
      correlations = tibble()
  )
    
  elements <- slotNames(analysis@parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = analysis@parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
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