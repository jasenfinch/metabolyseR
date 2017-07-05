
removeMethods <- function(method = NULL, description = F){
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
    
    descriptions = list(
      sample = list(description = '',
                    arguments = c(cls = '',
                                  samples = '')),
      class = list(description = '',
                   arguments = c(cls = '',
                                 classes = ''))
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

