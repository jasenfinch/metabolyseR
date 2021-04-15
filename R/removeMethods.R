#' removeSamples
#' @rdname removeSamples
#' @description Remove samples from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param idx info column containing sample indexes
#' @param samples sample indexes to remove
#' @export

setMethod('removeSamples',signature = 'AnalysisData',
          function(d,idx = 'fileOrder', samples = c()){
            dat(d) <- dat(d)[!(unlist(sinfo(d)[,idx]) %in% samples),]
            sinfo(d) <- sinfo(d)[!(unlist(sinfo(d)[,idx]) %in% samples),]
            return(d)
          }
)

#' removeClasses
#' @rdname removeClasses
#' @description Remove classes from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class information
#' @param classes classes to remove
#' @export

setMethod('removeClasses',signature = 'AnalysisData',
          function(d,cls = 'class', classes = c()){
            dat(d) <- dat(d)[!(unlist(sinfo(d)[,cls]) %in% classes),]
            sinfo(d) <-  sinfo(d)[!(unlist(sinfo(d)[,cls]) %in% classes),]
            return(d)
          }
)

#' removeFeatures
#' @rdname removeFeatures
#' @description Remove features from an AnalysisData object.
#' @param d S4 object of class AnalysisData
#' @param features features to remove
#' @export

setMethod('removeFeatures',signature = 'AnalysisData',
          function(d,features = character()){
            dat(d) <- dat(d)[,!(colnames(dat(d)) %in% features)]
            return(d)
          }
)

removeMethods <- function(method = NULL, description = FALSE){
  methods <- list(
    samples = removeSamples,
    classes = removeClasses,
    features = removeFeatures 
  )
  
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Remove method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- methods[[method]]
    }
 
  return(method)
}

