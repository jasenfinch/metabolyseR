#' QCMethods
#' @importFrom missForest missForest
#' @export

QCMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('occupancyFilter', 'impute','RSDfilter','removeQC'),collapse = ' '))
  } else {
    methods <- list(
      occupancyFilter = function(dat,occupancy = 2/3){
        dat$Data <- occDrop(dat$Data,rep(1,nrow(dat$Data)),occupancy)
        return(dat)
      },
      impute = imputeMethods('all'),
      RSDfilter = function(dat,RSDthresh = 0.5){
        RSD <- apply(dat$Data,2,function(y){sd(y)/mean(y)})
        dat$Data <- dat$Data[,RSD <= RSDthresh]
        return(dat)
      },
      removeQC = function(dat,cls = 'Class'){
        dat$Data <- dat$Data[which(dat$Info[,cls] == 'QC'),]
        dat$Info <- dat$Info[which(dat$Info[,cls] == 'QC'),]
        return(dat)
      }
    ) 
    method <- methods[[method]]
    return(method)
  }
}