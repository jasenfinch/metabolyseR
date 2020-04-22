#' plotPCA
#' @rdname plotPCA
#' @description Plot principle component analysis results of pre-treated data.
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for sample labelling
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param title plot title
#' @param legend TRUE/FALSE should a legend be plotted. Useful for many classes. Defaults to TRUE.
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Ignored if \code{legend = FALSE}.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importFrom ggplot2 scale_shape_manual geom_hline geom_vline
#' @importFrom stringr str_c
#' @importFrom stats prcomp
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
#' plotPCA(analysis,cls = 'day')
#' }
#' @export

setMethod('plotPCA',signature = 'AnalysisData',
          function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2', ellipses = T, title = 'Principle Component Analysis (PCA)', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            pca <- prcomp(dat(analysis),scale. = scale,center = center)
            
            info <- sinfo(analysis) %>%
              select(cls) %>%
              mutate(!!cls := factor(!!sym(cls)))
            
            var <- pca$sdev
            var <- round(var^2/sum(var^2) * 100,2)
            names(var) <- colnames(pca$x)
            
            pca <- pca$x %>%
              as_tibble() %>%
              select(xAxis = all_of(xAxis),yAxis = all_of(yAxis)) %>%
              bind_cols(info)
            
            if (!is.null(label)) {
              pca <- pca %>%
                mutate(Label = sinfo(analysis)[,label] %>% unlist())
            }
            
            pl <- pca %>%
              ggplot(aes(x = xAxis,y  = yAxis)) +
              geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
              geom_vline(xintercept = 0,linetype = 2,colour = 'grey')
            
            if (isTRUE(ellipses)) {
              pl <- pl +
                stat_ellipse(aes(fill = !!sym(cls)),alpha = 0.3,geom = 'polygon',type = 'norm')
            }
            
            if (!is.null(label)) {
              pl <- pl +
                geom_text_repel(aes(label = Label),size = labelSize)
            }
            
            classLength <- info %>%
              unique() %>%
              nrow()
            
            if (classLength <= 12) {
              pl <- pl + 
                scale_colour_ptol() +
                scale_fill_ptol()
            } else {
              if (classLength %% 12 == 0) {
                pal <- rep(ptol_pal()(12),classLength / 12)
              } else {
                pal <- c(rep(ptol_pal()(12),floor(classLength / 12)),ptol_pal()(12)[1:(classLength %% 12)])
              }
              pl <- pl + 
                scale_colour_manual(values = pal) +
                scale_fill_manual(values = pal)
            }
            
            if (classLength > 6) {
              sym <- 0:25
              if (classLength / max(sym) == 1) {
                val <- sym
              }
              if (classLength / max(sym) < 1) {
                val <- sym[1:classLength]
              }
              if (classLength / max(sym) > 1) {
                if (classLength %% max(sym) == 0) {
                  val <- rep(sym,classLength / max(sym))
                } else {
                  val <- c(rep(sym,floor(classLength / max(sym))),sym[1:(classLength %% max(sym))])
                }
              }
              pl <- pl + scale_shape_manual(values = val)
            }
            
            pl <- pl +
              geom_point(aes(colour = !!sym(cls),shape = !!sym(cls))) +
              theme_bw() +
              labs(title = title,
                   x = str_c(xAxis,' (Var: ',var[xAxis],'%)'),
                   y = str_c(yAxis,' (Var: ',var[yAxis],'%)')) +
              coord_fixed()
            
            if (legend == TRUE) {
              pl <- pl +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold'),
                      legend.title = element_text(face = 'bold'),
                      legend.position = legendPosition,
                      panel.grid = element_blank())
            } else {
              pl <- pl +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold'),
                      legend.title = element_text(face = 'bold'),
                      legend.position = 'none',
                      panel.grid = element_blank())
            }
            
            return(pl)
          }
)

#' @rdname plotPCA
#' @export

setMethod('plotPCA',signature = 'Analysis',
          function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2', ellipses = T, title = 'Principle Component Analysis (PCA)', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            if (ncol(analysis@preTreated %>% dat()) > 0) {
              d <- analysis@preTreated
            } else {
              d <- analysis@rawData
            }
            
            plotPCA(d, cls = cls, label = label, scale = scale, center = center, xAxis = xAxis, yAxis = yAxis, ellipses = ellipses, title = title, legend = legend, legendPosition = legendPosition, labelSize = labelSize)
          }
)