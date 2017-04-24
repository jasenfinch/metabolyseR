#' transformMethods
#' @export

transformMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('center','auto','range','pareto','vast','level','log','log10','sqrt','asinh','TICnorm'),collapse = ' '))
  } else {
  methods <- list(
    
    center = function(dat){
      apply(dat,2,function(x){x - mean(x,na.rm = T)})
    },
    
    auto = function(dat){
      apply(dat,2,function(x){x / sd(x,na.rm = T)})
    },
    
    range = function(dat){
      apply(dat,2,function(x){x / (max(x,na.rm = T) - min(x,na.rm = T))})
    },
    
    pareto = function(dat){
      apply(dat,2,function(x){x / mean(x,na.rm = T)/sqrt(sd(x,na.rm = T))})
    },
    
    vast = function(dat){
      apply(dat,2,function(x){x * mean(x,na.rm = T)/sd(x,na.rm = T)^2})
    },
    
    level = function(dat){
      apply(dat,2,function(x){x / mean(x,na.rm = T)})
    },
    
    log = function(dat, add = 1){
      log(dat)
    },
    
    log10 = function(dat, add = 1){
      log10(dat + add)
    },
    
    sqrt = function(dat){
      sqrt(dat)
    },
    
    asinh = function(dat){
      asinh(dat)
    },
    
    TICnorm = function(dat){
      apply(dat,2,function(x,y){x/y},y = rowSums(dat))
    }
  )
  
  method <- methods[[method]]
  return(method)
  }
}
