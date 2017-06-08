#' removeMethods
#' @export

removeMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('sample','class'),collapse = ' '))
  } else {
    methods <- list(
      sample = function(dat,params){
        dat <- dat[!(info$fileOrder %in% params$samples),]
        info <-  info[!(info$fileOrder %in% params$samples),]
        return(dat)
      },
      
      class = function(dat,params){
        dat$dat <- dat$dat[!(dat$info[,params$cls] %in% params$classes),]
        dat$info <-  dat$info[!(dat$info[,params$cls] %in% params$classes),]
        return(dat)
      })
    method <- methods[[method]]
    return(method)
  }
}

