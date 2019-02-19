#' plotClassification
#' @rdname plotClassification
#' @description Plot classification results.
#' @param analysis object of class Analysis containing analysis results
#' @param method results of classifier to plot
#' @importFrom ggplot2 aes_string geom_errorbarh theme element_text
#' @examples \dontrun{
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat','classification'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p) 
#' plotClassification(analysis)
#' }
#' @importFrom dplyr n
#' @export

setMethod('plotClassification',signature = 'Analysis',
          function(analysis, method = 'randomForest'){
            analysisPlot <- new('AnalysisPlot')
            
            analysisPlot@func <- function(analysisPlot){
              analysisPlot@data$Data %>%
                filter(Method == analysisPlot@data$method) %>%
                group_by(Pairwise,Measure) %>%
                summarise(Mean = mean(Value), SD = sd(Value),SE = sd(Value)/sqrt(n())) %>%
                ggplot(aes_string(x = 'Mean',y = 'Pairwise',xmin = 'Mean - SE',xmax = 'Mean + SE')) +
                geom_errorbarh(colour = '#3399FF',height = 0.3) +
                geom_point() +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                facet_wrap(~Measure,scales = 'free_x') +
                xlab('') +
                ylab('')
            }
            
            analysisPlot@data <- list(Data = classificationResults(analysis), method = method)
            analysisPlot@plot <- analysisPlot@func(analysisPlot)
            return(analysisPlot)
          })
