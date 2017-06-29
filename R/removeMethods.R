
removeMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('sample','class'),collapse = ' '))
  } else {
    methods <- list(
      sample = function(dat,cls = 'fileOrder', samples = NULL){
        dat$Data <- dat$Data[!(unlist(dat$Info[,cls]) %in% samples),]
        dat$Info <-  dat$Info[!(unlist(dat$Info[,cls]) %in% samples),]
        return(dat)
      },
      
      class = function(dat,cls = 'class', classes = NULL){
        dat$Data <- dat$Data[!(unlist(dat$Info[,cls]) %in% classes),]
        dat$Info <-  dat$Info[!(unlist(dat$Info[,cls]) %in% classes),]
        return(dat)
      })
    method <- methods[[method]]
    return(method)
  }
}

