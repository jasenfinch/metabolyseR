#' metabolyse 
#' @description Analyse data based on specified analysis elements.
#' @param  data tibble or data.frame containing data to analyse
#' @param info tibble or data.frame containing data info or meta data
#' @param parameters an object of AnalysisParameters class containing 
#' parameters for analysis. Default calls \code{analysisParameters()}
#' @param verbose should output be printed to the console 
#' @importFrom methods slotNames slot
#' @importFrom tibble tibble as_tibble 
#' @importFrom utils packageVersion
#' @importFrom cli console_width
#' @importFrom crayon yellow green
#' @importFrom lubridate seconds_to_period
#' @seealso \linkS4class{AnalysisParameters} \link{analysisParameters}
#' @examples 
#' library(metaboData)
#' 
#' ## Generate analysis parameters
#' p <- analysisParameters()
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
#' @export

metabolyse <- function(data,
                       info,
                       parameters = analysisParameters(), 
                       verbose = TRUE){
  version <- packageVersion('metabolyseR') %>% as.character()
  analysisStart <- date()
  
  if (verbose == TRUE) {
    startTime <- proc.time()
    message(
      blue('\nmetabolyseR '),
      ' ',
      red(str_c('v',version)),
      ' ',
      analysisStart)
    message(str_c(rep('_',console_width()),collapse = ''))
    params <- parameters %>%
      {capture.output(print(.))} %>%
      {
        .[1] <- yellow(.[1])
        .
      } %>%
      str_c(collapse = '\n')
    message(params)
    message(str_c(rep('_',console_width()),collapse = ''))
  }
  
  analysis <- new('Analysis',
                  log = list(
                    packageVersion = version,
                    analysis = analysisStart,
                    verbose = verbose),
                  parameters = parameters,
                  raw = analysisData(data,info),
                  `pre-treated` = new('AnalysisData'),
                  modelling = list(),
                  correlations = tibble()
  )
  
  elements <- analysisElements()
  elements <- elements[map_dbl(elements,
                               ~{length(slot(analysis@parameters,
                                             .))}) > 0]
  
  for (i in elements) {
    method <- get(i)
    analysis <- analysis %>% method() 
  }
  
  if (verbose == TRUE) {
    endTime <- proc.time()
    elapsed <- {endTime - startTime} %>%
      .[3] %>%
      round(1) %>%
      seconds_to_period() %>%
      str_c('[',.,']')
    
    message(str_c(rep('_',console_width()),collapse = ''))
    message('\n',green('Complete! '),elapsed,'\n')
  }
  return(analysis)
}
