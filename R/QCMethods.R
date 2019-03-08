#' @importFrom missForest missForest
#' @importFrom utils capture.output
#' @importFrom stats sd

QCMethods <- function(method = NULL, description = F){

    methods <- list(
      occupancyFilter = function(dat,cls = 'class', QCidx = 'QC', occupancy = 2/3){
        method <- occupancyMethods('maximum')
        QC <- dat
        QC$Data <- QC$Data[QC$Info[,cls] == QCidx,]
        QC$Info <- QC$Info[QC$Info[,cls] == QCidx,]
        QC <- method(QC,cls,occupancy)
        dat$Data <- dat$Data[,colnames(dat$Data) %in% colnames(QC$Data)]
        return(dat)
      },
      impute = function(dat, cls = 'class', QCidx = 'QC', occupancy = 2/3, parallel = 'variables', nCores = detectCores(), clusterType = 'PSOCK', seed = 1234){
        set.seed(seed)
        QC <- dat$Data[dat$Info[,cls] == QCidx,]
        QC <- apply(QC,2,function(x){x[x == 0] <- NA;return(x)})
        QC[which(QC == 0)] <- NA
        if (nCores > 1) {
          cl <- makeCluster(nCores,type = clusterType)
          registerDoParallel(cl)
          capture.output(QC <- missForest(QC,parallelize = parallel)$ximp)  
        } else {
          capture.output(QC <- missForest(QC)$ximp)  
        }
        dat$Data[dat$Info[,cls] == QCidx,] <- QC
        return(dat)
      },
      RSDfilter = function(dat,cls = 'class', QCidx = 'QC', RSDthresh = 0.5){
        QC <- dat$Data[dat$Info[,cls] == QCidx,]
        RSD <- apply(QC,2,function(y){sd(y)/mean(y)})
        dat$Data <- dat$Data[,RSD <= RSDthresh]
        return(dat)
      },
      removeQC = function(dat,cls = 'class', QCidx = 'QC'){
        dat$Data <- dat$Data[!(dat$Info[,cls] == QCidx),]
        dat$Info <- dat$Info[!(dat$Info[,cls] == QCidx),]
        return(dat)
      }
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