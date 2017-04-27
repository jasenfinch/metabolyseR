#' preTreatMethods
#' @export

preTreatMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('removeSample','removeClass','QCfilter','occupancyFilter','transform'),collapse = ' '))
  } else {
    methods <- list(
      
      removeSample = function(dat,pars){
        dat$Data <- dat$Data[-pars$removeSample,]
        dat$Info <- dat$Info[-pars$removeSample,]
        return(dat)
      },
      
      removeClass = function(dat,pars){
       dat$Data <- dat$Data[-which(dat$Info[,pars$cls] == pars$removeClass),] 
       dat$Info <- dat$Info[-which(dat$Info[,pars$cls] == pars$removeClass),] 
       return(dat)
      },
      
      QCfilter = function(dat,pars){
        for (i in 1:length(names(pars))) {
         method <- QCMethods(names(pars)[i])
         p <- formals(method)
         p[names(pars[[i]])] <- pars[[i]]
         formals(method) <- p
         dat <- method(dat)
        }
        return(dat)
      },
      
      occupancyFilter = function(dat,pars){
        dat$Data <- occDrop(dat$Data,dat$Info[,pars$cls],pars$occupancy)
        return(dat)
      },
      
      impute = function(dat,pars){
        method <- imputeMethods(names(pars)[1])
        p <- formals(method)
        p[names(pars[[1]])] <- pars[[1]]
        formals(method) <- p
        dat <- method(dat)
        return(dat)
      },
      
      transfrom = function(dat,pars){
        for (i in 1:length(names(pars))) {
          method <- QCMethods(names(pars)[i])
          p <- formals(method)
          p[names(pars[[i]])] <- pars[[i]]
          formals(method) <- p
          dat$Data <- method(dat$Data)
        }
        return(dat) 
      }
    ) 
    method <- methods[[method]]
    return(method)
  }
}