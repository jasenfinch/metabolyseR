#' analysisElements
#' @description Return the analysis elements available in metabolyseR.
#' @export

analysisElements <- function(){
  new('AnalysisParameters') %>%
    slotNames()
}

#' analysisParameters
#' @description Initiate default analysis parameters for analysis elements.
#' @param elements character vector containing elements for analysis. Default includes all available elements from \code{analysisElements}.
#' @importFrom parallel detectCores
#' @importFrom methods new
#' @export

analysisParameters <- function(elements = analysisElements()){
  
  if (!is.character(elements)) {
    stop('Argument "elements" should be a character vector.',call. = FALSE)
  }
  
  if (FALSE %in% (elements %in% analysisElements())) {
    elements <- analysisElements() %>%
      str_c('"',.,'"')
    stop(str_c('Vector elements of argument "elements" should be one of ',str_c(elements,collapse = ', '),'.'),call. = FALSE)
  }
  
  preTreat <- list()
  modelling <- list()
  correlations <- list()
  
  if ('pre-treatment' %in% elements) {
    preTreat <- list(QC = list(occupancyFilter = as.list(formals(QCMethods('occupancyFilter'))[-1]),
                               impute = as.list(formals(QCMethods('impute'))[-1]),
                               RSDfilter = as.list(formals(QCMethods('RSDfilter'))[-1]),
                               removeQC = as.list(formals(QCMethods('removeQC'))[-1])
    ), 
    occupancyFilter = list(maximum = as.list(formals(occupancyMethods('maximum'))[-1])),
    impute = list(class = as.list(formals(imputeMethods('class'))[-1])),
    transform = list(TICnorm = as.list(formals(transformMethods('TICnorm'))[-1]))
    )
  }
  
  if ('modelling' %in% elements) {
    modelling <- modellingParameters('randomForest')
  }
  if ('correlations' %in% elements) {
    correlations <- list(
      method = 'pearson',
      pAdjustMethod = 'bonferroni',
      corPvalue = 0.05
    )
  }
  
  p <- new('AnalysisParameters',
           `pre-treatment` = preTreat,
           modelling = modelling,
           correlations = correlations
  )
  
  return(p)
}


setMethod('parameters',signature = 'Analysis',
          function(x){
            x@parameters
          }
)

setMethod('parameters',signature = 'AnalysisParameters',
          function(x,element){
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(str_c('Argument "element" should be one of ',str_c(elements,collapse = ', '),'.'),call. = FALSE)
            }
            slot(x,element)
          }
)

setMethod('parameters<-',signature = 'AnalysisParameters',
          function(x,element,value){
            slot(x,element) <- value
            return(x)
          }
)
