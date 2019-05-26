#' QCoccupancy
#' @rdname QCoccupancy
#' @description QC maximum occupancy filter.
#' @param dat S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param occupancy occupancy threshold for filtering
#' @export

setMethod('QCoccupancy',signature = 'AnalysisData',
          function(dat,cls = 'class', QCidx = 'QC', occupancy = 2/3){
            method <- occupancyMethods('maximum')
            QC <- dat
            QC@data <- QC %>% 
              dat() %>% 
              .[QC %>% info() %>% .[,cls] == QCidx,]
            QC@info <- QC %>%
              info() %>%
              .[QC %>% info() %>% .[,cls] == QCidx,]
            QC <- method(QC,cls,occupancy)
            dat@data <- dat %>% 
              dat() %>%
              .[,colnames(dat$Data) %in% colnames(QC$Data)]
            return(dat)
          }
)

#' QCimpute
#' @rdname QCimpute
#' @description QC imputation.
#' @param dat S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param occupancy occupancy threshold for imputation
#' @param parallel parallel type to use. See `?missForest` for details
#' @param nCores number of cores for parallisation
#' @param clusterType cluster type for parallisation
#' @param seed random number seed
#' @importFrom missForest missForest
#' @importFrom utils capture.output
#' @export

setMethod('QCimpute',signature = 'AnalysisData',
          function(dat, cls = 'class', QCidx = 'QC', occupancy = 2/3, parallel = 'variables', nCores = detectCores(), clusterType = 'PSOCK', seed = 1234){
            set.seed(seed)
            QC <- dat %>%
              dat() %>%
            .[{dat %>% info() %>% .[,cls]} == QCidx,]
            QC <- apply(QC,2,function(x){x[x == 0] <- NA;return(x)})
            QC[which(QC == 0)] <- NA
            if (nCores > 1) {
              cl <- makeCluster(nCores,type = clusterType)
              registerDoParallel(cl)
              capture.output(QC <- missForest(QC,parallelize = parallel)$ximp)  
              stopCluster(cl)
            } else {
              capture.output(QC <- missForest(QC)$ximp)  
            }
            dat@data[{dat %>% info() %>% .[,cls]} == QCidx,] <- QC
            return(dat)
          }
)

#' QCrsdFilter
#' @rdname QCrsdFilter
#' @description QC relative standard deviation (RSD) filtering..
#' @param dat S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param RSDthresh RSD threshold for filtering
#' @importFrom stats sd
#' @export

setMethod('QCrsdFilter',signature = 'AnalysisData',
          function(dat,cls = 'class', QCidx = 'QC', RSDthresh = 0.5){
            QC <- dat %>%
              dat() %>%
              .[{dat %>% info() %>% .[,cls]} == QCidx,]
            RSD <- apply(QC,2,function(y){sd(y)/mean(y)})
            dat@data <- dat %>%
              dat() %>%
              .[,RSD <= RSDthresh]
            return(dat)
          }
)

#' QCremove
#' @rdname QCremove
#' @description Remove QC samples.
#' @param dat S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @importFrom stats sd
#' @export

setMethod('QCremove',signature = 'AnalysisData',
          function(dat,cls = 'class', QCidx = 'QC'){
            dat@data <- dat %>%
              dat() %>%
              .[!({dat %>% info() %>% .[,cls]} == QCidx),]
            dat@info <- dat %>%
              info() %>%
              .[!({dat %>% info() %>% .[,cls]} == QCidx),]
            return(dat)
          }
)

QCMethods <- function(method = NULL, description = F){
  
  methods <- list(
    occupancyFilter = QCoccupancy,
    impute = QCimpute,
    RSDfilter = QCrsdFilter,
    removeQC = QCremove
  ) 
  
  descriptions <- list(
    occupancyFilter = list(description = 'Filter variables based on occupancy in QC samples',
                           arguments = c(cls = 'info column to use for class labels',
                                         QCidx = 'QC sample label',
                                         occupancy = 'occupancy threshold for filtering')),
    impute = list(description = 'Impute missing values in QC samples',
                  arguments = c(cls = 'info column to use for class labels',
                                QCidx = 'QC sample label',
                                occupancy = 'occupancy threshold for imputation',
                                parallel = 'parallel type to use. See `?missForest` for details',
                                nCores = 'number of cores for parallisation',
                                clusterType = 'cluster type for parallisation',
                                seed = 'random number seed')
    ),
    RSDfilter = list(description = 'Filter variables based on their relative standard deviation in QC samples',
                     arguments = c(cls = 'info column to use for class labels',
                                   QCidx = 'QC sample label',
                                   RSDthreshold = 'RSD threshold for filtering')),
    removeQC = list(description = 'Remove QC samples',
                    arguments = c(cls = 'info column to use for class labels',
                                  QCidx = 'QC sample label'))
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      method <- descriptions[[method]]
    }
  }
  return(method)
}