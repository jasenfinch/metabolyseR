#' analysisParameters
#' @export

analysisParameters <- function(elements = c('preTreat','classification','featureSelection','correlations')){
  
  if ('preTreat' %in% elements) {
    preTreat <- list(QC = list(occupancyFilter = list(),
                               impute = list(),
                               RSDfilter = list(),
                               removeQC = list()
                               ), 
                     impute = list(class = list()),
                     transform = list(TICnorm = list())
    )
  } else {
    preTreat <- list()
  }
  if ('classification' %in% elements) {
    classification <- list(
      cls = 'class' ,
      method = c('randomForest'),
      pars = list(sampling = "boot",niter = 10,nreps = 10, strat = T,div = 2/3), 
      nCores = 1,
      clusterType = 'FORK'
    )
  } else {
    classification <- list()
  }
  if ('featureSelection' %in% elements) {
    featureSelection <- list(
      method = 'fs.rf',
      cls = 'class',
      pars = NULL, 
      nCores = 1, 
      clusterType = 'FORK'
    )
  } else {
    featureSelection <- list()
  }
  if ('correlations' %in% elements) {
    correlations <- list(
      pAdjustMethod = 'bonferroni',
      corPvalue = 0.05
    )
  } else {
    correlations <- list()
  }
  
  new('AnalysisParameters',
      preTreat = preTreat,
      classification = classification,
      featureSelection = featureSelection,
      correlations = correlations
  )
}