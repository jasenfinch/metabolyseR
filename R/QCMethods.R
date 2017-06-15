#' QCMethods
#' @importFrom missForest missForest
#' @export

QCMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('occupancyFilter', 'impute','RSDfilter','removeQC'),collapse = ' '))
  } else {
    methods <- list(
      occupancyFilter = function(dat,cls = 'class', occupancy = 2/3){
        method <- occupancyMethods('maximum')
        QC <- method(as.matrix(dat$Data[dat$Info[,cls] == 'QC',]),rep(1,nrow(as.matrix(dat$Data[dat$Info[,cls] == 'QC',]))),occupancy)
        dat$Data <- dat$Data[,colnames(dat$Data) %in% colnames(QC)]
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
    method <- methods[[method]]
    return(method)
  }
}