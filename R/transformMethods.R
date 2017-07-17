
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
    center = list(description = 'Mean centering',
                  arguments = c(`''` = '')),
    auto = list(description = 'Auto scaling',
                arguments = c(`''` = '')),
    range = list(description = 'Range scaling',
                 arguments = c(`''` = '')),
    pareto = list(description = 'Pareto scaling',
                  arguments = c(`''` = '')),
    vast = list(description = 'Vast scaling',
                arguments = c(`''` = '')),
    level = list(description = 'Level scaling',
                 arguments = c(`''` = '')),
    log = list(description = 'Natural log scaling',
               arguments = c(add = 'value to add prior to transformation')),
    log10 = list(description = 'Log10 scaling',
                 arguments = c(add = 'value to add prior to transformation')),
    sqrt = list(description = 'Square root scaling',
                arguments = c(`''` = '')),
    asinh = list(description = 'Arc-sine scaling',
                 arguments = c(`''` = '')),
    TICnorm = list(description = 'Total ion count normalisation',
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
