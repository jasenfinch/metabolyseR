#' preTreatMethods
#' @export

preTreatMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('remove','transform','impute','QC'),collapse = ' '))
  } else {
    methods <- list(
      remove = function(params){
        lapply(params,removeMethods)
      },
      
      transform = function(params){
        lapply(params,transformMethods)
      },
      
      impute = function(params){
        lapply(params,imputeMethods)
      },
      
      QC = function(params){
        lapply(params,QCMethods)
      }
    )
    method <- methods[[method]]
    return(method)
  }
}