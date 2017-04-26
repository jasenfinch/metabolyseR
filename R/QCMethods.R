#' QCMethods
#' @importFrom missForest missForest
#' @export

QCMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('occupancyFilter', 'impute','RSDfilter','removeQC'),collapse = ' '))
  } else {
    methods <- list(
      occupancyFilter = function(dat,occupancy = 2/3){
        occDrop(dat,rep(1,nrow(dat)),occupancy)
      },
      impute = imputeMethods('all'),
      RSDfilter = function(dat,RSDthresh = 0.5){
        RSD <- apply(dat,2,function(y){sd(y)/mean(y)})
        dat <- dat[,RSD <= RSDthresh]
        return(dat)
      },
      removeQC = function(dat,info,cls){
        dat <- dat[which(info[,cls] == 'QC'),]
        info <- info[which(info[,cls] == 'QC'),]
        return(list(Info = info, Data = dat))
      }
    ) 
    method <- methods[[method]]
    return(method)
  }
}