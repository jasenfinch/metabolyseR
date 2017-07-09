
removeMethods <- function(method = NULL, description = F){
    methods <- list(
      sample = function(dat,idx = 'fileOrder', samples = c()){
        dat$Data <- dat$Data[!(unlist(dat$Info[,idx]) %in% samples),]
        dat$Info <-  dat$Info[!(unlist(dat$Info[,idx]) %in% samples),]
        return(dat)
      },
      
      class = function(dat,cls = 'class', classes = c()){
        dat$Data <- dat$Data[!(unlist(dat$Info[,cls]) %in% classes),]
        dat$Info <-  dat$Info[!(unlist(dat$Info[,cls]) %in% classes),]
        return(dat)
      })
    
    descriptions = list(
      sample = list(description = 'Remove samples',
                    arguments = c(idx = 'info column containing sample indices',
                                  samples = 'sample indices to remove')),
      class = list(description = 'Remove classes',
                   arguments = c(cls = 'info column containing class information',
                                 classes = 'classes to remove'))
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

