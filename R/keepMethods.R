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

#' keepFeatures
#' @rdname keepFeatures
#' @description Keep features from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param features features to keep
#' @export

setMethod('keepFeatures',signature = 'AnalysisData',
          function(d,features = character()){
            dat(d) <- dat(d)[,colnames(dat(d)) %in% features]
            return(d)
          }
)

keepMethods <- function(method = NULL){
  methods <- list(
    samples = keepSamples,
    classes = keepClasses,
    features = keepFeatures
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    if (!(method %in% names(methods))) {
      stop(str_c("Keep method '",
                 method,
                 "' not recognised. Available methods include: ",
                 str_c(str_c("'",names(methods),"'"),collapse = ' '),'.'))
    }
    method <- methods[[method]]
  }
  
  return(method)
}

