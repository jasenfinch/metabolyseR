#' Unsupervised random forest MDS plot
#' @rdname plotUnsupervisedRF
#' @description A multidimensional scaling (MDS) plot of unsupervised random forest analysis
#' @param x object of class `AnalysisData` or `Analysis`
#' @param cls sample information column to use for sample labelling
#' @param rf list of additional parameters to pass to `randomForest`
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param shape TRUE/FALSE use shape aesthetic for plot points. 
#' Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95% 
#' confidence ellipses for each class
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument 
#' of `ggplot2::theme`. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if `label` is `NULL`
#' @param type `raw` or `pre-treated` data to plot
#' @param ... arguments to pass to the appropriate method
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' ## Unsupervised random forest MDS plot
#' plotUnsupervisedRF(d,cls = 'day')
#' @export

setGeneric('plotUnsupervisedRF',
           function(
             x,
             cls = 'class', 
             rf = list(), 
             label = NULL, 
             shape = FALSE, 
             ellipses = TRUE, 
             seed = 1234, 
             title = '', 
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
           {
             standardGeneric('plotUnsupervisedRF')
           })

#' @rdname plotUnsupervisedRF
#' @importFrom stats cmdscale

setMethod('plotUnsupervisedRF', 
          signature = 'AnalysisData',
          function(x,
                   cls = 'class', 
                   rf = list(), 
                   label = NULL, 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   seed = 1234, 
                   title = '', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            
            rf <- randomForest(x,
                               cls = NULL,
                               rf = rf,
                               reps = 1,
                               seed = seed)
            
            plotMDS(rf,
                    cls = cls,
                    label = label,
                    shape = shape,
                    ellipses = ellipses,
                    title = title,
                    legendPosition = legendPosition,
                    labelSize = labelSize)
          }
)
              
#' @rdname plotUnsupervisedRF

setMethod('plotUnsupervisedRF',
          signature = 'Analysis',
          function(x,
                   cls = 'class', 
                   rf = list(), 
                   label = NULL, 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   seed = 1234, 
                   title = '', 
                   legendPosition = 'bottom', 
                   labelSize = 2,
                   type = 'raw'){
            
            if (!(type %in% c('raw','pre-treated'))) {
              stop(
                'Argument "type" should be one of "raw" or "pre-treated".',
                call. = FALSE)
            }
            
            if (type == 'pre-treated') {
              d <- x %>%
                preTreated()
            } else {
              d <- x %>%
                raw()
            }
            
            plotUnsupervisedRF(d,
                               cls = cls,
                               rf = rf,
                               label = label,
                               shape = shape, 
                               ellipses = ellipses,
                               seed = seed,
                               title = title,
                               legendPosition = legendPosition,
                               labelSize = labelSize)
          }
)
