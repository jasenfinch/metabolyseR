#' plotPCA
#' @rdname plotPCA
#' @description Plot principle component analysis results of pre-treated data.
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for sample labelling
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @importFrom ggplot2 scale_shape_manual geom_hline geom_vline
#' @importFrom stringr str_c
#' @importFrom stats prcomp
#' @examples 
#' 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)
#' plotPCA(analysis,cls = 'day')
#' @export

setMethod('plotPCA',signature = 'Analysis',
          function(analysis, cls = 'class', scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2'){
            analysisPlot <- new('AnalysisPlot')
            
            analysisPlot@func <- function(analysisPlot){
              pca <- analysisPlot@data$PCAresults
              
              info <- analysisPlot@data$Info %>%
                select(Class = analysisPlot@data$cls) %>%
                mutate(Class = factor(Class))
              
              var <- pca$sdev
              var <- round(var^2/sum(var^2) * 100,2)
              names(var) <- colnames(pca$x)
              
              pca <- pca$x %>%
                as_tibble() %>%
                select(xAxis = xAxis,yAxis = yAxis) %>%
                bind_cols(info)
              
              pl <- pca %>%
                ggplot(aes(x = xAxis,y  = yAxis,colour = Class,shape = Class)) +
                geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
                geom_vline(xintercept = 0,linetype = 2,colour = 'grey') +
                geom_point() +
                theme_bw() +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold'),
                      legend.title = element_text(face = 'bold')) +
                xlab(str_c(xAxis,' (Var: ',var[xAxis],'%)')) +
                ylab(str_c(yAxis,' (Var: ',var[yAxis],'%)')) +
                ggtitle('Principle Component Analysis (PCA)\nplot')
              
              classLength <- info %>%
                unique() %>%
                nrow()
              
              if (classLength <= 12) {
                pl <- pl + scale_colour_ptol()
              } else {
                if (classLength %% 12 == 0) {
                  pal <- rep(ptol_pal()(12),classLength / 12)
                } else {
                  pal <- c(rep(ptol_pal()(12),floor(classLength / 12)),ptol_pal()(12)[1:(classLength %% 12)])
                }
                pl <- pl + scale_colour_manual(values = pal)
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
              
              pl              
            }
            
            pca <- prcomp(preTreatedData(analysis),scale. = scale,center = center)
            
            analysisPlot@data <- list(Data = preTreatedData(analysis),
                                      Info = preTreatedInfo(analysis),
                                      PCAresults = pca,
                                      cls = cls)
            analysisPlot@plot <- analysisPlot@func(analysisPlot)
            return(analysisPlot)
          }
)