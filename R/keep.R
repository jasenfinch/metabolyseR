#' Keep samples, classes or features
#' @rdname keep
#' @description Retain samples, classes or features in an `AnalysisData` object.
#' @param d S4 object of class AnalysisData
#' @param idx info column containing sample indexes
#' @param samples sample indexes to keep
#' @param cls info column to use for class information
#' @param classes classes to keep
#' @param features features to remove
#' @return An S4 object of class `AnalysisData` with specified samples, classes or features retained.
#' @section Methods:
#' * `keepClasses`: Keep classes.
#' * `keepFeatures`: Keep features.
#' * `keepSamples`: Keep samples.
#' @examples 
#' library(metaboData)
#'  d <- analysisData(abr1$neg[,200:300],abr1$fact)
#'  
#'  ## Keep classes
#'  d %>% 
#'   keepClasses(cls = 'day',classes = 'H')
#'  
#'  ## Keep features
#'  d %>% 
#'   keepFeatures(features = c('N200','N201'))
#'  
#'  ## Keep samples
#'  d %>% 
#'   keepSamples(idx = 'injorder',samples = c(1,10))
#' @export

setGeneric("keepClasses", function(d,cls = 'class', classes = c()) {
  standardGeneric("keepClasses")
})

#' @rdname keep

setMethod('keepClasses',signature = 'AnalysisData',
          function(d,cls = 'class', classes = c()){
            dat(d) <- dat(d)[unlist(sinfo(d)[,cls]) %in% classes,]
            sinfo(d) <-  sinfo(d)[unlist(sinfo(d)[,cls]) %in% classes,]
            return(d)
          }
)

#' @rdname keep
#' @export

setGeneric("keepFeatures", function(d,features = character()) {
  standardGeneric("keepFeatures")
})

#' @rdname keep

setMethod('keepFeatures',signature = 'AnalysisData',
          function(d,features = character()){
            dat(d) <- dat(d)[,colnames(dat(d)) %in% features]
            return(d)
          }
)

#' @rdname keep
#' @export

setGeneric("keepSamples", function(d,idx = 'fileOrder', samples = c()) {
  standardGeneric("keepSamples")
})

#' @rdname keep

setMethod('keepSamples',signature = 'AnalysisData',
          function(d,idx = 'fileOrder', samples = c()){
            dat(d) <- dat(d)[unlist(sinfo(d)[,idx]) %in% samples,]
            sinfo(d) <- sinfo(d)[unlist(sinfo(d)[,idx]) %in% samples,]
            return(d)
          }
)
