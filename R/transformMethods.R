
transformMethods <- function(method = NULL, description = F){

  methods <- list(
    
    center = function(dat){
      dat$Data <- apply(dat$Data,2,function(x){x - mean(x,na.rm = T)})
      return(dat)
    },
    
    auto = function(dat){
      dat$Data <- apply(dat$Data,2,function(x){x / sd(x,na.rm = T)})
      return(dat)
    },
    
    range = function(dat){
      dat$Data <- apply(dat$Data,2,function(x){x / (max(x,na.rm = T) - min(x,na.rm = T))})
      return(dat)
    },
    
    pareto = function(dat){
      dat$Data <- apply(dat$Data,2,function(x){x / mean(x,na.rm = T)/sqrt(sd(x,na.rm = T))})
      return(dat)
    },
    
    vast = function(dat){
      dat$Data <- apply(dat$Data,2,function(x){x * mean(x,na.rm = T)/sd(x,na.rm = T)^2})
      return(dat)
    },
    
    level = function(dat){
      dat$Data <- apply(dat$Data,2,function(x){x / mean(x,na.rm = T)})
      return(dat)
    },
    
    log = function(dat, add = 1){
      dat$Data <- log(dat$Data + add)
      return(dat)
    },
    
    log10 = function(dat, add = 1){
      dat$Data <- log10(dat$Data + add)
      return(dat)
    },
    
    sqrt = function(dat){
      dat$Data <- sqrt(dat$Data)
      return(dat)
    },
    
    asinh = function(dat){
      dat$Data <- asinh(dat$Data)
      return(dat)
    },
    
    TICnorm = function(dat){
      dat$Data <- apply(dat$Data,2,function(x,y){x/y},y = rowSums(dat$Data))
      return(dat)
    }
  )
  
  descriptions = list(
    center = list(description = '',
                  arguments = c(`''` = '')),
    auto = list(description = '',
                arguments = c(`''` = '')),
    range = list(description = '',
                 arguments = c(`''` = '')),
    pareto = list(description = '',
                  arguments = c(`''` = '')),
    vast = list(description = '',
                arguments = c(`''` = '')),
    level = list(description = '',
                 arguments = c(`''` = '')),
    log = list(description = '',
               arguments = c(`''` = '')),
    log10 = list(description = '',
                 arguments = c(`''` = '')),
    sqrt = list(description = '',
                arguments = c(`''` = '')),
    asinh = list(description = '',
                 arguments = c(`''` = '')),
    TICnorm = list(description = '',
                   arguments = c(`''` = ''))
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
