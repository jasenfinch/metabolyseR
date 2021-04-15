#' Remove samples, classes or features
#' @rdname remove
#' @description Remove samples, classes or features from an AnalysisData object.
#' @param d S4 object of class `AnalysisData`
#' @param idx info column containing sample indexes
#' @param samples sample indexes to remove
#' @param cls info column to use for class information
#' @param classes classes to remove
#' @param features features to remove
#' @return An S4 object of class `AnalysisData` with samples, classes or features removed.
#' @section Methods:
#' * `removeClasses`: Remove classes.
#' * `removeFeatures`: Remove features.
#' * `removeSamples`: Remove samples.
#' @examples 
#'  d <- analysisData(abr1$neg[,200:300],abr1$fact)
#'  
#'  ## Remove classes
#'  d %>% removeClasses(cls = 'day',classes = 'H')
#'  
#'  ## Remove features
#'  d %>% removeFeatures(features = c('N200','N201'))
#'  
#'  ## Remove samples
#'  d %>% removeSamples(idx = 'injorder',samples = c(1,10))

setMethod('removeSamples',signature = 'AnalysisData',
          function(d,idx = 'fileOrder', samples = c()){
            dat(d) <- dat(d)[!(unlist(sinfo(d)[,idx]) %in% samples),]
            sinfo(d) <- sinfo(d)[!(unlist(sinfo(d)[,idx]) %in% samples),]
            return(d)
          }
)

#' @rdname remove

setMethod('removeClasses',signature = 'AnalysisData',
          function(d,cls = 'class', classes = c()){
            dat(d) <- dat(d)[!(unlist(sinfo(d)[,cls]) %in% classes),]
            sinfo(d) <-  sinfo(d)[!(unlist(sinfo(d)[,cls]) %in% classes),]
            return(d)
          }
)

#' @rdname remove

setMethod('removeFeatures',signature = 'AnalysisData',
          function(d,features = character()){
            dat(d) <- dat(d)[,!(colnames(dat(d)) %in% features)]
            return(d)
          }
)
