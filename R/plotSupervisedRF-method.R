#' plotSupervisedRF
#' @rdname plotSupervisedRF
#' @export

setMethod('plotSupervisedRF', signature = 'Analysis',
          function(analysis,cls = 'class',...){
            analysisPlot <- new('AnalysisPlot')
            
            analysisPlot@func <- function(analysisPlot){
              prox <- analysisPlot@data$RFresults$proximity
              
              distance <- {1 - rf$proximity} %>% 
                cmdscale() %>%
                as_tibble() %>%
                rename(`Dimension 1` = V1,`Dimension 2` = V2) %>%
                mutate(Class = analysisPlot@data$RFresults$y)
              
              distance %>%
                ggplot(aes(x = `Dimension 1`,y = `Dimension 2`,colour = Class)) +
                geom_hline(yintercept = 0,colour = 'lightgray',linetype = 2) +
                geom_vline(xintercept = 0,colour = 'lightgray',linetype = 2) +
                geom_point() +
                theme_bw() +
                ggtitle('MDS plot of a supervised\nrandom forest') +
                theme(plot.title = element_text(face = "bold"),
                      legend.title = element_text(face = "bold"),
                      axis.title = element_text(face = "bold"))
            }
            
            rf <- randomForest(preTreatedData(analysis)$Data,y = factor(unlist(preTreatedData(analysis)$Info[,cls])),proximity = T,...)
            
            analysisPlot@data <- list(Data = preTreatedData(analysis)$Data,Info = preTreatedData(analysis)$Info,RFresults = rf)
            
            analysisPlot@plot <- analysisPlot@func(analysisPlot)
            
            return(analysisPlot)
          }
)