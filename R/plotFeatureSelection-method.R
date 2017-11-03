#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point theme_bw facet_wrap guides ylab xlab
#' @importFrom stringr str_sub str_replace_all 
#' @importFrom ggthemes scale_colour_ptol ptol_pal
#' @export

setMethod('plotFeatureSelection',signature = 'Analysis',
          function(analysis, method = 'fs.rf', mz = T, modes = T) {
            featureSelection <- featureSelectionResults(analysis) %>%
              filter(Method == method) 
            
            if (modes == T) {
              featureSelection <- featureSelection %>%
                mutate(Mode = str_sub(Feature,1,1))
              featureSelection$Mode[featureSelection$Mode == 'n'] <- 'Negative'
              featureSelection$Mode[featureSelection$Mode == 'p'] <- 'Positive'
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