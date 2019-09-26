#' QCoccupancy
#' @rdname QCoccupancy
#' @description QC maximum occupancy filter.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param occupancy occupancy threshold for filtering
#' @export

setMethod('QCoccupancy',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC', occupancy = 2/3){
            method <- occupancyMethods('maximum')
            QC <- d
            dat(QC) <- QC %>% 
              dat() %>% 
              .[QC %>% sinfo() %>% .[,cls] == QCidx,]
            sinfo(QC) <- QC %>%
              sinfo() %>%
              .[QC %>% sinfo() %>% .[,cls] == QCidx,]
            QC <- method(QC,cls,occupancy)
            d@data <- d %>% 
              dat() %>%
              .[,colnames(d %>% dat()) %in% colnames(QC %>% dat())]
            return(d)
          }
)

#' QCimpute
#' @rdname QCimpute
#' @description QC imputation.
#' @param d S4 object of class AnalysisData
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
          function(d, cls = 'class', QCidx = 'QC', occupancy = 2/3, parallel = 'variables', nCores = detectCores() * 0.75, clusterType = getClusterType(), seed = 1234){
            set.seed(seed)
            QC <- d %>%
              dat() %>%
            .[{d %>% sinfo() %>% .[,cls]} == QCidx,]
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
            dat(d)[{d %>% sinfo() %>% .[,cls]} == QCidx,] <- QC
            return(d)
          }
)

#' QCrsdFilter
#' @rdname QCrsdFilter
#' @description QC relative standard deviation (RSD) filtering..
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param RSDthresh RSD threshold for filtering
#' @importFrom stats sd
#' @export

setMethod('QCrsdFilter',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC', RSDthresh = 0.5){
            QC <- d %>%
              dat() %>%
              .[{d %>% sinfo() %>% .[,cls]} == QCidx,]
            RSD <- apply(QC,2,function(y){sd(y)/mean(y)})
            dat(d) <- d %>%
              dat() %>%
              .[,RSD <= RSDthresh]
            return(d)
          }
)

#' QCremove
#' @rdname QCremove
#' @description Remove QC samples.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @importFrom stats sd
#' @export

setMethod('QCremove',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC'){
            dat(d) <- d %>%
              dat() %>%
              .[!({d %>% sinfo() %>% .[,cls]} == QCidx),]
            sinfo(d) <- d %>%
              sinfo() %>%
              .[!({d %>% sinfo() %>% .[,cls]} == QCidx),]
            return(d)
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
      if (!(method %in% names(methods))) {
        stop(str_c("QC method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("QC method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}