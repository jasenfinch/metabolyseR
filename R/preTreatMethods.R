
preTreatMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('remove','transform','impute','QC','occupancyFilter','aggregate','correction'),collapse = ' '))
  } else {
    methods <- list(
      remove = function(params){
        lapply(params,removeMethods)
      },
      
      keep = function(params){
        lapply(params,keepMethods)
      },
      
      transform = function(params){
        lapply(params,transformMethods)
      },
      
      impute = function(params){
        lapply(params,imputeMethods)
      },
      
      QC = function(params){
        lapply(params,QCMethods)
      },
      
      occupancyFilter = function(params){
        lapply(params,occupancyMethods)
      },
      
      aggregate = function(params){
        lapply(params,aggregateMethods)
      },
      
      correction = function(params){
        lapply(params,correctionMethods)
      }
    )
    
    if (!(method %in% names(methods))) {
      stop(str_c("Pre-treatment element '",
                 method,
                 "' not recognised. Available elements include: ",
                 str_c(str_c("'",names(methods),"'"),collapse = ' '),'.'))
    }
    
    method <- methods[[method]]
    return(method)
  }
}