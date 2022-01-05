#' Supervised random forest MDS plot
#' @rdname plotSupervisedRF
#' @description A multidimensional scaling (MDS) plot of supervised random forest analysis
#' @param x object of class `AnalysisData` or `Analysis` containing analysis results
#' @param cls information column to use for sample classes
#' @param rf list of additional parameters to pass to `randomForest`
#' @param label information column to use for sample labels. Set to `NULL` for no labels.
#' @param shape TRUE/FALSE use shape aesthetic for plot points. 
#' Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95% 
#' confidence ellipses for each class
#' @param ROC should receiver-operator characteristics be plotted?
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument 
#' of `ggplot2::theme`. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if `label` is `NULL`
#' @param type `raw` or `pre-treated` data to plot
#' @param ... arguments to pass to the appropriate method
#' @importFrom patchwork plot_annotation wrap_plots
#' @examples
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' ## Supervised random forest MDS plot
#' plotSupervisedRF(d,cls = 'day')
#' @export

setGeneric('plotSupervisedRF',
           function(
             x, 
             cls = 'class', 
             rf = list(), 
             label = NULL,
             shape = FALSE, 
             ellipses = TRUE, 
             ROC = TRUE, 
             seed = 1234, 
             title = '', 
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
             standardGeneric('plotSupervisedRF'))

#' @rdname plotSupervisedRF

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
            
            rf <- try(randomForest(x,
                               cls = cls,
                               rf = rf,
                               reps = 1,
                               seed = seed))
            
            if (class(rf) != 'try-error') {
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
            } else {
              warning('Issues detected, skipping plotting of supervised random forest.',call. = FALSE)
            }
          }
)

#' @rdname plotSupervisedRF

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
