#' metabolyse 
#' @description Analyse data based on specified analysis elements.
#' @param  data tibble or data.frame containing data to analyse
#' @param info tibble or data.frame containing data info or meta data
#' @param parameters an object of AnalysisParameters class containing parameters for analysis. Default calls \code{analysisParameters()}
#' @importFrom magrittr %>%
#' @importFrom methods slotNames slot
#' @importFrom tibble tibble as_tibble 
#' @importFrom utils packageVersion
#' @importFrom cli rule
#' @importFrom crayon yellow green
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

metabolyse <- function(data,info,parameters = analysisParameters(),verbose = T){
  version <- packageVersion('metabolyseR')
  analysisStart <- date()
  analysis <- new('Analysis',
      log = list(packageVersion = version,analysis = analysisStart,verbose = verbose),
      parameters = parameters,
      rawData = list(Info = as_tibble(info),Data = as_tibble(data)),
      preTreated = list(),
      classification = tibble(),
      featureSelection = tibble(),
      correlations = tibble()
  )
  
  if (verbose == T) {
    cat('\n',blue('metabolyseR'),' ',red(str_c('v',version)),' ',analysisStart,'\n',sep = '')
    print(parameters)
  }
    
  elements <- slotNames(analysis@parameters)
  elements <- elements[sapply(elements,function(x,parameters){length(slot(parameters,x))},parameters = analysis@parameters) > 0]
  
  for (i in elements) {
    method <- get(i)
    analysis <- analysis %>% method() 
  }
  
  if (verbose == T) {
    cat('\n',green('Finished!'),sep = '')
  }
  return(analysis)
}