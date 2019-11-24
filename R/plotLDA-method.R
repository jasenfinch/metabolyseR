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
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param title plot title
#' @param legend TRUE/FALSE should a legend be plotted. Useful for many classes. Defaults to TRUE.
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Ignored if \code{legend = FALSE}.
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
#' @importFrom ggplot2 stat_ellipse coord_fixed ylab
#' @export

setMethod('plotLDA',signature = 'AnalysisData',
          function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2', ellipses = T, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA)', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            info <- sinfo(analysis) %>%
              select(cls)
            colnames(info)[1] <- 'Class'
            
            lda <- nlda(dat(analysis),cl = info$Class,scale = scale,center = center)
            
            classLength <- lda$cl %>%
              unique() %>%
              length()
            
            tw <- lda$Tw %>%
              round(2)
            
            lda <- lda$x %>%
              as_tibble() %>%
              bind_cols(info) %>%
              mutate(Class = factor(Class))
            
            
            
            if (classLength > 2) {
              lda <- lda %>%
                select(Class,xAxis = xAxis,yAxis = yAxis)
              
              if (!is.null(label)) {
                lda <- lda %>%
                  mutate(Label = info[,label] %>% unlist())
              }
              
              pl <- lda %>%
                ggplot(aes(x = xAxis,y  = yAxis)) +
                geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
                geom_vline(xintercept = 0,linetype = 2,colour = 'grey')
              
              if (isTRUE(ellipses)) {
                pl <- pl +
                  stat_ellipse(aes(fill = Class),alpha = 0.3,geom = 'polygon',type = 'norm')
              }
              
              if (!is.null(label)) {
                pl <- pl +
                  geom_text_repel(aes(label = Label),size = labelSize)
              }
              
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
                geom_point(aes(colour = Class,shape = Class)) +
                theme_bw() +
                labs(title = title,
                     x = str_c(xAxis,' (Tw: ',tw[xAxis],')'),
                     y = str_c(yAxis,' (Tw: ',tw[yAxis],')')) +
                coord_fixed()
              
              if (legend == TRUE) {
                pl <- pl +
                  theme(plot.title = element_text(face = 'bold'),
                        axis.title = element_text(face = 'bold'),
                        legend.title = element_text(face = 'bold'),
                        legend.position = 'bottom'
                  )
              } else {
                pl <- pl +
                  theme(plot.title = element_text(face = 'bold'),
                        axis.title = element_text(face = 'bold'),
                        legend.title = element_text(face = 'bold'),
                        legend.position = 'none'
                  )
              }
            } else {
              pl <- lda %>%
                ggplot(aes(x = Class,y = DF1,colour = Class,shape = Class)) +
                geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
                geom_point() +
                scale_colour_ptol() +
                theme_bw() +
                guides(colour = F,shape = F) +
                ylab(str_c('DF1',' (Tw: ',tw['DF1'],')'))
            }
            
            return(pl)
          }
) 

#' @rdname plotLDA
#' @export

setMethod('plotLDA',signature = 'Analysis',
          function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2', ellipses = T, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA)', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            if (ncol(analysis@preTreated %>% dat()) > 0) {
              d <- analysis@preTreated
            } else {
              d <- analysis@rawData
            }
            
            plotLDA(d, cls = cls, label = label, scale = scale, center = center, xAxis = xAxis, yAxis = yAxis, ellipses = ellipses, title = title, legend = legend, legendPosition = legendPosition, labelSize = labelSize)
          }
)