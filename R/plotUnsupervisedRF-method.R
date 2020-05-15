#' plotUnsupervisedRF
#' @rdname plotUnsupervisedRF
#' @param x object of class Analysis containing analysis results
#' @param cls info column to use for sample labelling
#' @param rf list of additional parameters to pass to randomForest
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param shape TRUE/FALSE use shape aesthetic for plot points. Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95\% confidence ellipses for each class
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importFrom stats cmdscale
#' @export

setMethod('plotUnsupervisedRF', signature = 'AnalysisData',
          function(x,cls = 'class', rf = list(), label = NULL, shape = FALSE, ellipses = TRUE, seed = 1234, title = '', legendPosition = 'bottom', labelSize = 2){
            
            rf <- randomForest(x,cls = NULL,rf = rf,reps = 1,seed = seed,nCores = 1,clusterType = getClusterType())
            
            plotMDS(rf[[1]],cls = cls,label = label,shape = shape,ellipses = ellipses,title = title,legendPosition = legendPosition,labelSize = labelSize)
          }
)
              
#' @rdname plotUnsupervisedRF
#' @export

setMethod('plotUnsupervisedRF', signature = 'Analysis',
          function(x,cls = 'class', rf = list(), label = NULL, shape = FALSE, ellipses = TRUE, seed = 1234, title = '', legendPosition = 'bottom', labelSize = 2){
            
            if (nFeatures(x,type = 'pre-treated') > 0) {
              d <- preTreated(x)
            } else {
              d <- raw(x)
            }
            
            plotUnsupervisedRF(d,cls = cls,rf = rf,label = label,shape = shape, ellipses = ellipses,seed = seed,title = title,legendPosition = legendPosition,labelSize = labelSize)
          }
)