#' plotUnsupervisedRF
#' @rdname plotUnsupervisedRF
#' @export

setMethod('plotUnsupervisedRF', signature = 'Analysis',
          function(analysis,cls = 'class',seed = 1234, ...){
            analysisPlot <- new('AnalysisPlot')
            
            analysisPlot@func <- function(analysisPlot){
              cls <- analysisPlot@data$cls
              prox <- analysisPlot@data$RFresults$proximity
              
              distance <- {1 - rf$proximity} %>% 
                cmdscale() %>%
                as_tibble() %>%
                rename(`Dimension 1` = V1,`Dimension 2` = V2) %>%
                bind_cols(analysisPlot@data$Info[,analysisPlot@data$cls]) %>%
                select(`Dimension 1`,`Dimension 2`,Class = cls)
              
              pl <- distance %>%
                ggplot(aes(x = `Dimension 1`,y = `Dimension 2`,colour = Class,shape = Class)) +
                geom_hline(yintercept = 0,colour = 'lightgray',linetype = 2) +
                geom_vline(xintercept = 0,colour = 'lightgray',linetype = 2) +
                geom_point() +
                theme_bw() +
                ggtitle('MDS plot of an unsupervised\nrandom forest') +
                theme(plot.title = element_text(face = "bold"),
                      legend.title = element_text(face = "bold"),
                      axis.title = element_text(face = "bold"))
              
              classLength <- distance$Class %>%
                unique() %>%
                length()
              
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
            
            set.seed(seed)
            rf <- randomForest(preTreatedData(analysis)$Data,proximity = T,...)
            
            analysisPlot@data <- list(Data = preTreatedData(analysis)$Data,Info = preTreatedData(analysis)$Info,RFresults = rf, cls = cls)
            
            analysisPlot@plot <- analysisPlot@func(analysisPlot)
            
            return(analysisPlot)
          }
)