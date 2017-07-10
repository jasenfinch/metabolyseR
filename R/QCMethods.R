#' @importFrom missForest missForest
#' @importFrom utils capture.output
#' @importFrom stats sd

QCMethods <- function(method = NULL, description = F){

    methods <- list(
      occupancyFilter = function(dat,cls = 'class', idx = 'QC', occupancy = 2/3){
        method <- occupancyMethods('maximum')
        QC <- dat
        QC$Data <- QC$Data[QC$Info[,cls] == idx,]
        QC$Info <- QC$Info[QC$Info[,cls] == idx,]
        QC <- method(QC,cls,occupancy)
        dat$Data <- dat$Data[,colnames(dat$Data) %in% colnames(QC$Data)]
        return(dat)
      },
      impute = function(dat, cls = 'class', idx = 'QC', occupancy = 2/3){
        QC <- dat$Data[dat$Info[,cls] == idx,]
        QC <- apply(QC,2,function(x){x[x == 0] <- NA;return(x)})
        QC[which(QC == 0)] <- NA
        capture.output(QC <- missForest(QC)$ximp)
        dat$Data[dat$Info[,cls] == idx,] <- QC
        return(dat)
      },
      RSDfilter = function(dat,cls = 'class', idx = 'QC', RSDthresh = 0.5){
        QC <- dat$Data[dat$Info[,cls] == idx,]
        RSD <- apply(QC,2,function(y){sd(y)/mean(y)})
        dat$Data <- dat$Data[,RSD <= RSDthresh]
        return(dat)
      },
      removeQC = function(dat,cls = 'class', idx = 'QC'){
        dat$Data <- dat$Data[!(dat$Info[,cls] == idx),]
        dat$Info <- dat$Info[!(dat$Info[,cls] == idx),]
        return(dat)
      }
    ) 
    
    descriptions <- list(
      occupancyFilter = list(description = 'Filter variables based on occupancy on QC samples',
                             arguments = c(cls = 'info column to use for class labels',
                                           idx = 'QC sample label',
                                           occupancy = 'occupancy threshold for filtering')),
      impute = list(description = 'Impute missing values in QC samples',
                    arguments = c(cls = 'info column to use for class labels',
                                  idx = 'QC sample label',
                                  occupancy = 'occupancy threshold for imputation')),
      RSDfilter = list(description = 'Filter variables based on their relative standard deviation in QC samples',
                       arguments = c(cls = 'info column to use for class labels',
                                     idx = 'QC sample label',
                                     RSDthreshold = 'RSD threshold for filtering')),
      removeQC = list(description = 'Remove QC samples',
                      arguments = c(cls = 'info column to use for class labels',
                                    idx = 'QC sample label'))
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