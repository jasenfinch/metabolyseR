#' keepSamples
#' @rdname keepSamples
#' @description Keep samples from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param idx info column containing sample indexes
#' @param samples sample indexes to keep
#' @export

setMethod('keepSamples',signature = 'AnalysisData',
          function(d,idx = 'fileOrder', samples = c()){
            dat(d) <- dat(d)[unlist(sinfo(d)[,idx]) %in% samples,]
            sinfo(d) <- sinfo(d)[unlist(sinfo(d)[,idx]) %in% samples,]
            return(d)
          }
)

#' keepClasses
#' @rdname keepClasses
#' @description Keep classes from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class information
#' @param classes classes to keep
#' @export

setMethod('keepClasses',signature = 'AnalysisData',
          function(d,cls = 'class', classes = c()){
            dat(d) <- dat(d)[unlist(sinfo(d)[,cls]) %in% classes,]
            sinfo(d) <-  sinfo(d)[unlist(sinfo(d)[,cls]) %in% classes,]
            return(d)
          }
)

#' keepVariables
#' @rdname keepVariables
#' @description Keep variables from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param variables variables to keep
#' @export

setMethod('keepVariables',signature = 'AnalysisData',
          function(d,variables = character()){
            dat(d) <- dat(d)[,colnames(dat(d)) %in% variables]
            return(d)
          }
)

keepMethods <- function(method = NULL, description = F){
  methods <- list(
    samples = keepSamples,
    classes = keepClasses,
    variables = keepVariables 
  )
  
  descriptions = list(
    samples = list(description = 'keep samples',
                   arguments = c(idx = 'info column containing sample indexes',
                                 samples = 'sample indices to keep')),
    classes = list(description = 'keep classes',
                   arguments = c(cls = 'info column containing class information',
                                 classes = 'classes to keep')),
    variables = list(description = 'keep variables',
                     arguments = c(variables = 'variables to keep'))
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

