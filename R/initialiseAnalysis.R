#' startAnalysis 
#' @export

startAnalysis <- function(data,info){
  new('Analysis',
      log = list(analysis = date()),
      parameters = list(
        preTreat = list(cls = 'class',
                            occupancy = 2/3, 
                            QCfilter = T, 
                            RSDthresh = 0.5, 
                            removeQC = T, 
                            QCimpute = T, 
                            classImpute = T, 
                            normTIC = T, 
                            logTrans = F, 
                            removeSample = NULL, 
                            removeClass = NULL, 
                            add = 1),
        classification = list(
          cls = 'class' ,
          method = c('randomForest','nlda','svm'),
          pars = list(sampling = "boot",niter = 10,nreps = 10, strat = T,div = 2/3), 
          nCores = 1,
          clusterType = 'FORK'
        ),
        featureSelection = list(
          method = 'fs.rf',
          cls = 'class',
          pars = NULL, 
          nCores = 1, 
          clusterType = 'FORK'
        ),
        correlations = list(
          pAdjustMethod = 'bonferroni',
          corPvalue = 0.05
        )
      ),
      rawData = list(Info = info,Data = data),
      preTreated = list(),
      classification = list(),
      featureSelection = list()
  )
}