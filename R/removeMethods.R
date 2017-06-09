#' removeMethods
#' @export

removeMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('sample','class'),collapse = ' '))
  } else {
    methods <- list(
      sample = function(dat,samples = NULL){
        dat$Data <- dat$Data[!(dat$Info$fileOrder %in% samples),]
        dat$Info <-  dat$Info[!(dat$Info$fileOrder %in% samples),]
        return(dat)
      },
      
      class = function(dat,cls = NULL, classes = NULL){
        dat$Data <- dat$Data[!(dat$Info[,cls] %in% classes),]
        dat$Info <-  dat$Info[!(dat$Info[,cls] %in% classes),]
        return(dat)
      })
    method <- methods[[method]]
    return(method)
  }
}

