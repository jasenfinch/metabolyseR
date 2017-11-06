#' plotClassification
#' @rdname plotClassification
#' @description Plot classification results.
#' @param analysis object of class Analysis containing analysis results
#' @param method results of classifier to plot
#' @importFrom ggplot2 aes_string
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
#' @export

setMethod('plotClassification',signature = 'Analysis',
          function(analysis, method = 'randomForest'){
            classificationResults(analysis) %>%
              filter(Method == method) %>%
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
          })
