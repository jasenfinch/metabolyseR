#' plotLDA
#' @rdname plotLDA
#' @description Plot linear discriminant analysis resultus of pre-treated data
#' @param analysis object of class Analysis or AnalysisData containing analysis results
#' @param cls info column to use for sample labelling
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @param shape TRUE/FALSE use shape aesthetic for plot points. Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95\% confidence ellipses for each class
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @examples 
#' \dontrun{
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters(c('preTreat'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)
#' plotLDA(analysis,cls = 'day')
#' }
#' @importFrom ggplot2 stat_ellipse coord_fixed ylab scale_colour_manual
#' @export

setMethod('plotLDA',signature = 'AnalysisData',
          function(analysis, cls = 'class', label = NULL, scale = TRUE, center = TRUE, xAxis = 'DF1', yAxis = 'DF2', shape = FALSE, ellipses = TRUE, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA)', legendPosition = 'bottom', labelSize = 2){
            
            classLength <- clsLen(analysis,cls)
            
            if (classLength < 2) {
              stop('More than 1 class needed for PC-LDA.',call. = FALSE)
            }
            
            info <- analysis %>%
              clsExtract(cls) %>%
              factor()
            
            lda <- nlda(dat(analysis),cl = info,scale = scale,center = center)
            
            tw <- lda$Tw %>%
              round(2)
            
            lda <- lda$x %>%
              as_tibble() %>%
              mutate(!!cls := info)
            
            if (classLength > 2) {
              lda <- lda %>%
                select(all_of(c(cls,xAxis,yAxis)))
              
              if (!is.null(label)) {
                lda <- lda %>%
                  bind_cols(info %>%
                              select(label()))
              }
              
              classLength <- clsLen(analysis,cls)
              
              pl <- scatterPlot(lda,cls,xAxis,yAxis,ellipses,shape,label,labelSize,legendPosition,classLength,title,str_c(xAxis,' (Tw: ',tw[xAxis],')'),str_c(yAxis,' (Tw: ',tw[yAxis],')'))
            } else {
              pl <- lda %>%
                {
                  ggplot(.,aes(x = !!sym(cls),y = DF1)) +
                    geom_hline(yintercept = 0,linetype = 2,colour = 'grey')
                } %>%
                plotShape(cls,shape,classLength) %>%
                plotColour(classLength) %>%
                plotTheme(legendPosition = 'none',title,xLabel = cls,yLabel = str_c('DF1',' (Tw: ',tw['DF1'],')'))
            }
            
            return(pl)
          }
) 

#' @rdname plotLDA
#' @export

setMethod('plotLDA',signature = 'Analysis',
          function(analysis, cls = 'class', label = NULL, scale = TRUE, center = TRUE, xAxis = 'DF1', yAxis = 'DF2', shape = FALSE, ellipses = TRUE, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA)', legendPosition = 'bottom', labelSize = 2){
            if (ncol(preTreatedData(analysis)) > 0) {
              d <- preTreated(analysis)
            } else {
              d <- raw(analysis)
            }
            
            plotLDA(d, cls = cls, label = label, scale = scale, center = center, xAxis = xAxis, yAxis = yAxis, shape = shape, ellipses = ellipses, title = title, legendPosition = legendPosition, labelSize = labelSize)
          }
)