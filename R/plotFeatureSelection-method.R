#' plotFeatureSelection
#' @rdname plotFeatureSelection
#' @description Plot feature selection results
#' @param analysis object of class Analysis containing analysis results
#' @param method results of feature selection method to use
#' @param mz \code{TRUE} if features are m/z
#' @param modes split modes if present
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point theme_bw facet_wrap guides ylab xlab
#' @importFrom stringr str_sub str_replace_all 
#' @importFrom ggthemes scale_colour_ptol ptol_pal
#' @examples \dontrun{
#' 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat','featureSelection'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p) 
#' plotFeatureSelection(analysis)
#' }
#' @export

setMethod('plotFeatureSelection',signature = 'Analysis',
          function(analysis, method = 'fs.rf', mz = T, modes = T) {
            featureSelection <- featureSelectionResults(analysis) %>%
              filter(Method == method) 
            
            if (modes == T) {
              featureSelection <- featureSelection %>%
                mutate(Mode = str_sub(Feature,1,1))
            }
            
            if (mz == T) {
              featureSelection <- featureSelection %>%
                mutate(Index = as.numeric(str_replace_all(Feature,'[:alpha:]','')))
            } else {
              featureSelection <- featureSelection %>%
                rowid_to_column(var = 'Index')
            }
            
            featureSelection <- featureSelection %>%
              mutate(`-log10(Score)` = -log10(Score))
            
            
            pl <- featureSelection %>%
              ggplot(aes(x = Index,y = `-log10(Score)`)) +
              scale_colour_ptol() +
              guides(colour = FALSE) +
              theme_bw() +
              ylab(expression(-log[10]*FPR))
            
            if (modes == T) {
              pl <- pl + 
                geom_point(aes(colour = Mode)) +
                facet_wrap(~Mode)
            } else {
              pl <- pl + geom_point(colour = ptol_pal()(1))
            }
            
            if (mz == T)  {
              pl <- pl + xlab('m/z')
            }
            
            pl
          }
)