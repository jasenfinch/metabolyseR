#' @importFrom missForest missForest
#' @importFrom utils capture.output
#' @importFrom stats sd

QCMethods <- function(method = NULL, description = F){

    methods <- list(
      occupancyFilter = function(dat,cls = 'class', occupancy = 2/3){
        method <- occupancyMethods('maximum')
        QC <- dat
        QC$Data <- QC$Data[QC$Info[,cls] == 'QC',]
        QC$Info <- QC$Info[QC$Info[,cls] == 'QC',]
        QC <- method(QC,cls,occupancy)
        dat$Data <- dat$Data[,colnames(dat$Data) %in% colnames(QC$Data)]
        return(dat)
      },
      impute = function(dat, cls = 'class', occupancy = 2/3){
        QC <- dat$Data[dat$Info[,cls] == 'QC',]
        QC <- apply(QC,2,function(x){x[x == 0] <- NA;return(x)})
        QC[which(QC == 0)] <- NA
        capture.output(QC <- missForest(QC)$ximp)
        dat$Data[dat$Info[,cls] == 'QC',] <- QC
        return(dat)
      },
      RSDfilter = function(dat,cls = 'class', RSDthresh = 0.5){
        QC <- dat$Data[dat$Info[,cls] == 'QC',]
        RSD <- apply(QC,2,function(y){sd(y)/mean(y)})
        dat$Data <- dat$Data[,RSD <= RSDthresh]
        return(dat)
      },
      removeQC = function(dat,cls = 'class'){
        dat$Data <- dat$Data[!(dat$Info[,cls] == 'QC'),]
        dat$Info <- dat$Info[!(dat$Info[,cls] == 'QC'),]
        return(dat)
      }
    ) 
    
    descriptions <- list(
      occupancyFilter = list(description = '',
                             arguments = c(cls = '',
                                           occupancy = '')),
      impute = list(description = '',
                    arguments = c(cls = '',
                                  occupancy = '')),
      RSDfilter = list(description = '',
                       arguments = c(cls = '',
                                     RSDthreshold = '')),
      removeQC = list(description = '',
                      arguments = c(cls = ''))
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