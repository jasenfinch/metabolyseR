#' Principle Component - Linear Discriminant Analysis plot
#' @rdname plotLDA
#' @description Plot linear discriminant analysis results of pre-treated data
#' @param analysis S4 object of class `AnalysisData` or `Analysis`
#' @param cls name of sample information column to use for class labels
#' @param label name of sample information column to use for sample labels. Set to NULL for no labels.
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @param shape TRUE/FALSE use shape aesthetic for plot points. 
#' Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95\% 
#' confidence ellipses for each class
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument 
#' of `ggplot2::theme`. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if `label` is `NULL`
#' @param type `raw` or `pre-treated` data to plot
#' @param ... arguments to pass to the appropriate method
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact) %>% 
#'  occupancyMaximum(cls = 'day')
#' 
#' ## LDA plot
#' plotLDA(d,cls = 'day')
#' @export

setGeneric('plotLDA', 
           function(
             analysis, 
             cls = 'class', 
             label = NULL, 
             scale = TRUE, 
             center = TRUE, 
             xAxis = 'DF1', 
             yAxis = 'DF2', 
             shape = FALSE, 
             ellipses = TRUE, 
             title = 'PC-LDA', 
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
             standardGeneric('plotLDA'))

#' @rdname plotLDA
#' @importFrom ggplot2 stat_ellipse coord_fixed ylab scale_colour_manual

setMethod('plotLDA',
          signature = 'AnalysisData',
          function(analysis, 
                   cls = 'class', 
                   label = NULL,
                   scale = TRUE, 
                   center = TRUE, 
                   xAxis = 'DF1', 
                   yAxis = 'DF2', 
                   shape = FALSE, 
                   ellipses = TRUE,
                   title = 'PC-LDA', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            
            
            lda <- try(nlda(analysis,cls = cls,scale = scale,center = center))
            
            if (!is(lda,'try-error')) {
            tw <- lda@Tw %>%
              round(2)
            
            classLength <- clsLen(lda,cls = cls)
            
            lda <- lda@x %>%
              as_tibble() %>%
              mutate(!!cls := clsExtract(analysis,cls = cls))
            
            if (classLength > 2) {
              lda <- lda %>%
                select(all_of(c(cls,xAxis,yAxis)))
              
              if (!is.null(label)) {
                lda <- lda %>%
                  bind_cols(sinfo(analysis) %>%
                              select(all_of(label)))
              }
              
              pl <- scatterPlot(lda,
                                cls,
                                xAxis,
                                yAxis,
                                ellipses,
                                shape,
                                label,
                                labelSize,
                                legendPosition,
                                classLength,
                                title,
                                str_c(xAxis,' (Tw: ',tw[xAxis],')'),
                                str_c(yAxis,' (Tw: ',tw[yAxis],')'))
            } else {
              pl <- lda %>%
                {
                  ggplot(.,aes(x = !!sym(cls),y = DF1)) +
                    geom_hline(yintercept = 0,linetype = 2,colour = 'grey')
                } %>%
                plotShape(cls,shape,classLength) %>%
                plotColour(classLength) %>%
                plotTheme(legendPosition = 'none',
                          title,xLabel = cls,
                          yLabel = str_c('DF1',' (Tw: ',tw['DF1'],')'))
            }
            
            return(pl)
            } else {
              warning('Errors encounted in PC-LDA, skipping plotting of PC-LDA.',call. = FALSE)
            }
          }
) 

#' @rdname plotLDA

setMethod('plotLDA',
          signature = 'Analysis',
          function(analysis, 
                   cls = 'class', 
                   label = NULL, 
                   scale = TRUE, 
                   center = TRUE, 
                   xAxis = 'DF1', 
                   yAxis = 'DF2', 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   title = 'PC-LDA', 
                   legendPosition = 'bottom', 
                   labelSize = 2,
                   type = c('pre-treated',
                            'raw')){

            type <- match.arg(
              type,
              choices = c(
                'pre-treated',
                'raw'
              )
            )
            
            if (type == 'pre-treated') {
              d <- analysis %>%
                preTreated()
            } 
            
            if (type == 'raw'){
              d <- analysis %>%
                raw()
            }
            
            plotLDA(d, 
                    cls = cls, 
                    label = label, 
                    scale = scale, 
                    center = center, 
                    xAxis = xAxis, 
                    yAxis = yAxis, 
                    shape = shape, 
                    ellipses = ellipses, 
                    title = title, 
                    legendPosition = legendPosition, 
                    labelSize = labelSize)
          }
)
