#' plotSupervisedRF
#' @rdname plotSupervisedRF
#' @param x object of class Analysis or AnalysisData containing analysis results
#' @param cls info column to use for sample classes
#' @param rf list of additional parameters to pass to randomForest
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param ROC should reciever-operator characteristics be plotted?
#' @param seed random number seed
#' @param title plot title
#' @param legend TRUE/FALSE should a legend be plotted. Useful for many classes. Defaults to TRUE.
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Ignored if \code{legend = FALSE}.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importFrom patchwork plot_annotation wrap_plots
#' @examples 
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters('preTreat')
#' p@preTreat <- list(
#'   occupancyFilter = list(maximum = list()),
#'   transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)  
#' 
#' plotSupervisedRF(analysis,cls = 'day',label = 'name')
#' @export

setMethod('plotSupervisedRF',signature = 'AnalysisData',
          function(x, cls = 'class', rf = list(), label = NULL, ellipses = T, ROC = T, seed = 1234, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            rf <- randomForest(x,cls = cls,rf = rf,reps = 1,seed = seed,nCores = 1,clusterType = getClusterType())
            
            pl <- plotMDS(rf[[1]],cls = cls,label = label,ellipses = ellipses,title = '',legend = legend,legendPosition = legendPosition,labelSize = labelSize) +
              labs(caption = str_c('Margin: ',rf[[1]]@results$measures$.estimate[4] %>% round(3)))
            
            if (isTRUE(ROC)) {
              pl <- pl + plotROC(rf[[1]],legend = legend) + plot_annotation(title = title,theme = theme(plot.title = element_text(face = 'bold')))
            } else {
              pl <- pl + labs(title = title)
            }
            
            return(pl)
          }
)

#' @rdname plotSupervisedRF
#' @export

setMethod('plotSupervisedRF', signature = 'Analysis',
          function(x, cls = 'class', rf = list(), label = NULL, ellipses = T, ROC = T, seed = 1234, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            if (ncol(x@preTreated %>% dat()) > 0) {
              d <- x@preTreated
            } else {
              d <- x@rawData
            }
            
            plotSupervisedRF(d,cls = cls,rf = rf,label = label,ellipses = ellipses,ROC = ROC,seed = seed,title = title,legend = legend,legendPosition = legendPosition,labelSize = labelSize)
          }
)