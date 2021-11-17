#' Perform an analysis
#' @rdname metabolyse
#' @description Perform analyses containing multiple analysis element steps.
#' @param data tibble or data.frame containing data to analyse
#' @param info tibble or data.frame containing data info or meta data
#' @param parameters an object of AnalysisParameters class containing 
#' parameters for analysis. Default calls \code{analysisParameters()}
#' @param verbose should output be printed to the console 
#' @param analysis an object of class Analysis containing previous 
#' analysis results
#' @return An S4 object of class `Analysis`.
#' @details 
#' Routine analyses are those that are often made up of numerous steps where parameters have likely already been previously established. 
#' The emphasis here is on convenience with as little code as possible required.
#' In these analyses, the necessary analysis elements, order and parameters are first prepared and then the analysis routine subsequently performed in a single step.
#' The `metabolyse` function provides this utility, where the metabolome data, sample meta information and analysis parameters are provided.
#' The `reAnalyse` method can be used to perform further analyses on the results.
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
#' ## Change "cls" parameters 
#' changeParameter(p,'cls') <- 'day'
#' 
#' ## Run analysis using a subset of the abr1 negative mode data set
#' analysis <- metabolyse(abr1$neg[,1:200],
#'                        abr1$fact,
#'                        p)
#'                        
#' ## Re-analyse to include correlation analysis
#' analysis <- reAnalyse(analysis,
#'                       parameters = analysisParameters('correlations'))
#'
#' print(analysis)
#' 
#' @importFrom methods slotNames slot
#' @importFrom tibble tibble as_tibble 
#' @importFrom utils packageVersion
#' @importFrom cli console_width
#' @importFrom crayon yellow green
#' @importFrom lubridate seconds_to_period
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

#' @rdname metabolyse
#' @export

setGeneric('reAnalyse',
           function(analysis,parameters = analysisParameters(),verbose = TRUE)
             standardGeneric('reAnalyse'))

#' @rdname metabolyse
#' @importFrom methods slot slot<-

setMethod('reAnalyse',signature = 'Analysis',function(analysis,
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
  elements <- elements[map_dbl(elements,
                               ~{length(slot(parameters,.x))}) > 0]
  
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
})
