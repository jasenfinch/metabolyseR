#' analysisParameters
#' @description Initiate default analysis parameters for analysis elements.
#' @param elements character vector containing elements for analysis (see Details). Default includes all available elements.
#' @details Analysis elements can include:
#' \itemize{
#' \item preTreat
#' \item classification
#' \item featureSelection
#' \item correlations
#' }
#' @importFrom parallel detectCores
#' @importFrom methods new
#' @export

analysisParameters <- function(elements = c('preTreat','classification','featureSelection','correlations')){
  
  if ('preTreat' %in% elements) {
    preTreat <- list(QC = list(occupancyFilter = as.list(formals(QCMethods('occupancyFilter'))[-1]),
                               impute = as.list(formals(QCMethods('impute'))[-1]),
                               RSDfilter = as.list(formals(QCMethods('RSDfilter'))[-1]),
                               removeQC = as.list(formals(QCMethods('removeQC'))[-1])
    ), 
    occupancyFilter = list(maximum = as.list(formals(occupancyMethods('maximum'))[-1])),
    impute = list(class = as.list(formals(imputeMethods('class'))[-1])),
    transform = list(TICnorm = as.list(formals(transformMethods('TICnorm'))[-1]))
    )
  } else {
    preTreat <- list()
  }
  if ('classification' %in% elements) {
    classification <- list(
      cls = 'class',
      method = c('randomForest'),
      pairwises = character(),
      pars = list(sampling = "boot",niter = 10,nreps = 10, strat = T), 
      nCores = detectCores(),
      clusterType = 'PSOCK'
    )
  } else {
    classification <- list()
  }
  if ('featureSelection' %in% elements) {
    featureSelection <- list(
      method = 'fs.rf',
      cls = 'class',
      pairwises = character(),
      pars = list(fs.rf = as.list(formals(fsMethods('fs.rf'))[-1])), 
      nCores = detectCores(), 
      clusterType = 'PSOCK'
    )
  } else {
    featureSelection <- list()
  }
  if ('correlations' %in% elements) {
    correlations <- list(
      method = 'pearson',
      pAdjustMethod = 'bonferroni',
      corPvalue = 0.05
    )
  } else {
    correlations <- list()
  }
  
  p <- new('AnalysisParameters',
      preTreat = preTreat,
      classification = classification,
      featureSelection = featureSelection,
      correlations = correlations
  )
  
  if (.Platform$OS.type != 'windows') {
    p <- changeParameter('clusterType','FORK',p)
  }
  
  p <- changeParameter('nCores',{detectCores() * 0.75} %>% round(),p)
  
  return(p)
}