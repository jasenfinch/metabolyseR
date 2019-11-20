#' plotUnsupervisedRF
#' @rdname plotUnsupervisedRF
#' @param x object of class Analysis containing analysis results
#' @param cls info column to use for sample labelling
#' @param rf list of additional parameters to pass to randomForest
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param seed random number seed
#' @param title plot title
#' @param legend TRUE/FALSE should a legend be plotted. Useful for many classes. Defaults to TRUE.
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Ignored if \code{legend = FALSE}.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importFrom stats cmdscale
#' @export

setMethod('plotUnsupervisedRF', signature = 'AnalysisData',
          function(x,cls = 'class', rf = list(), label = NULL, ellipses = T, seed = 1234, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            rf <- randomForest(x,cls = NULL,rf = rf,reps = 1,seed = seed,nCores = 1,clusterType = getClusterType())
            
            plotMDS(rf[[1]],cls = cls,label = label,ellipses = ellipses,title = title,legend = legend,legendPosition = legendPosition,labelSize = labelSize)
          }
)
              
#' @rdname plotUnsupervisedRF
#' @export

setMethod('plotUnsupervisedRF', signature = 'Analysis',
          function(x,cls = 'class', rf = list(), label = NULL, ellipses = T, seed = 1234, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            if (ncol(x@preTreated %>% dat()) > 0) {
              d <- x@preTreated
            } else {
              d <- x@rawData
            }
            
            plotUnsupervisedRF(d,cls = cls,rf = rf,label = label,ellipses = ellipses,seed = seed,title = title,legend = legend,legendPosition = legendPosition,labelSize = labelSize)
          }
)