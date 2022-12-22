#' Principle Component Analysis plot
#' @rdname plotPCA
#' @description Plot Principle Component Analysis results.
#' @param analysis object of class `AnalysisData` or `Analysis`
#' @param cls name of class information column to use for sample labelling
#' @param label name of class information column to use for sample labels. Set to NULL for no labels.
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @param shape TRUE/FALSE use shape aesthetic for plot points. 
#' Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95\%
#'  confidence ellipses for each class
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument 
#' of \code{ggplot2::theme}. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @param type `raw` or `pre-treated` data to plot
#' @param ... arguments to pass to the appropriate method
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact) %>% 
#'  occupancyMaximum(cls = 'day')
#' 
#' ## PCA plot
#' plotPCA(d,cls = 'day')
#' @export

setGeneric('plotPCA', 
           function(
    analysis, 
    cls = 'class', 
    label = NULL, 
    scale = TRUE, 
    center = TRUE, 
    xAxis = 'PC1', 
    yAxis = 'PC2', 
    shape = FALSE, 
    ellipses = TRUE, 
    title = 'PCA',
    legendPosition = 'bottom', 
    labelSize = 2,
    ...)
    standardGeneric('plotPCA'))

#' @rdname plotPCA
#' @importFrom ggplot2 scale_shape_manual geom_hline geom_vline
#' @importFrom stringr str_c
#' @importFrom stats prcomp

setMethod('plotPCA',
          signature = 'AnalysisData',
          function(analysis,
                   cls = 'class', 
                   label = NULL, 
                   scale = TRUE, 
                   center = TRUE, 
                   xAxis = 'PC1', 
                   yAxis = 'PC2', 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   title = 'Principle Component Analysis (PCA)', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            
            pca <- prcomp(dat(analysis),scale. = scale,center = center)
            
            info <- sinfo(analysis) %>%
              select(all_of(cls)) %>%
              mutate(!!cls := factor(!!sym(cls)))
            
            var <- pca$sdev
            var <- round(var^2/sum(var^2) * 100,2)
            names(var) <- colnames(pca$x)
            
            pca <- pca$x %>%
              as_tibble() %>%
              select(all_of(c(xAxis,yAxis))) %>%
              bind_cols(info)
            
            if (!is.null(label)) {
              pca <- pca %>%
                bind_cols(sinfo(analysis) %>%
                            select(all_of(label)))
            }
            
            classLength <- clsLen(analysis,cls)
            
            pl <- scatterPlot(
              pca,
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
              str_c(xAxis,
                    ' (Var: ',
                    var[xAxis],'%)'),
              str_c(yAxis,
                    ' (Var: ',
                    var[yAxis],'%)'))
            
            return(pl)
          }
)

#' @rdname plotPCA

setMethod('plotPCA',
          signature = 'Analysis',
          function(analysis, 
                   cls = 'class', 
                   label = NULL, 
                   scale = TRUE, 
                   center = TRUE, 
                   xAxis = 'PC1', 
                   yAxis = 'PC2', 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   title = 'PCA', 
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
            
            plotPCA(d, 
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
