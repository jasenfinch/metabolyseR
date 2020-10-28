#' plotSupervisedRF
#' @rdname plotSupervisedRF
#' @param x object of class Analysis or AnalysisData containing analysis results
#' @param cls info column to use for sample classes
#' @param rf list of additional parameters to pass to randomForest
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param shape TRUE/FALSE use shape aesthetic for plot points. 
#' Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95\% 
#' confidence ellipses for each class
#' @param ROC should reciever-operator characteristics be plotted?
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument 
#' of \code{ggplot2::theme}. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importFrom patchwork plot_annotation wrap_plots
#' @examples 
#' \dontrun{
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
#' }
#' @export

setMethod('plotSupervisedRF',
          signature = 'AnalysisData',
          function(x, 
                   cls = 'class', 
                   rf = list(), 
                   label = NULL, 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   ROC = TRUE, 
                   seed = 1234, 
                   title = '', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            
            rf <- randomForest(x,
                               cls = cls,
                               rf = rf,
                               reps = 1,
                               seed = seed,
                               nCores = 1,
                               clusterType = getClusterType())
            
            pl <- plotMDS(rf,
                          cls = cls,
                          label = label,
                          ellipses = ellipses,
                          title = '',
                          legendPosition = legendPosition,
                          labelSize = labelSize) +
              labs(
                caption = str_c('Margin: ',
                                rf@results$measures$.estimate[4] %>% 
                                  round(3)))
            
            if (isTRUE(ROC) & rf@type == 'classification') {
              pl <- pl + 
                plotROC(rf,legendPosition = legendPosition) + 
                plot_annotation(
                  title = title,
                  theme = theme(plot.title = element_text(face = 'bold')))
            } else {
              pl <- pl + labs(title = title)
            }
            
            return(pl)
          }
)

#' @rdname plotSupervisedRF
#' @export

setMethod('plotSupervisedRF', 
          signature = 'Analysis',
          function(x, 
                   cls = 'class', 
                   rf = list(), 
                   label = NULL, 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   ROC = TRUE, 
                   seed = 1234, 
                   title = '', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            
            if (ncol(x %>% dat(type = 'pre-treated')) > 0) {
              d <- preTreated(x)
            } else {
              d <- raw(x)
            }
            
            plotSupervisedRF(d,
                             cls = cls,
                             rf = rf,
                             label = label,
                             shape = shape,
                             ellipses = ellipses,
                             ROC = ROC,
                             seed = seed,
                             title = title,
                             legendPosition = legendPosition,
                             labelSize = labelSize)
          }
)