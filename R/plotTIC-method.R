#' plotTIC
#' @rdname plotTIC
#' @description Plot total ion counts of sample raw data.
#' @param analysis object of class Analysis or AnalysisData containing analysis results
#' @param by info column to plot against
#' @param colour info column to provide colour labels
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to the appropriate method
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 scale_fill_manual geom_boxplot
#' @importFrom stringr str_sub
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
#' plotTIC(analysis,by = 'injorder',colour = 'day')
#' }
#' @export

setMethod('plotTIC',signature = 'AnalysisData',
          function(analysis, by = 'injOrder', colour = 'block') {
            d <- dat(analysis)
            info <- sinfo(analysis)
            
            index <- info %>%
              select(Index = by,Colour = colour)
            
            d <- d %>%
              bind_cols(index) %>%
              mutate(Colour = factor(Colour)) %>%
              rowid_to_column(var = 'ID') %>%
              gather('Feature','Intensity',-ID,-Index,-Colour) %>%
              group_by(ID,Index,Colour) %>%
              summarise(TIC = sum(Intensity))
            
            classCheck <- d %>%
              group_by(Index) %>%
              summarise(Frequency = n())
            
            pl <- d %>%
              ggplot(aes(x = Index,y = TIC)) +
              theme_bw() +
              xlab(by)
            
            if (T %in% (classCheck$Frequency > 1)) {
              pl <- pl + 
                geom_boxplot(aes(group = Index),outlier.shape = NA,colour = 'black') +
                geom_point(aes(fill = Colour),shape = 21,position = 'jitter')
            } else {
              pl <- pl + geom_point(aes(fill = Colour),shape = 21)
            }
            
            if (nrow(classCheck) <= 12) {
              pl <- pl + scale_fill_ptol()
            } else {
              if (nrow(classCheck) %% 12 == 0) {
                pal <- rep(ptol_pal()(12),nrow(classCheck) / 12)
              } else {
                pal <- c(rep(ptol_pal()(12),floor(nrow(classCheck) / 12)),ptol_pal()(12)[1:(nrow(classCheck) %% 12)])
              }
              pl <- pl + scale_fill_manual(values = pal,name = colour)
            }
            
            pl + 
              theme(legend.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    panel.grid = element_blank())
          }
)

#' @rdname plotTIC
#' @export

setMethod('plotTIC',signature = 'Analysis',
          function(analysis, by = 'injOrder', colour = 'block', type = 'raw') {
            ty <- get(type)
            
            ty(analysis) %>%
              plotTIC(by = by,colour = colour)
          }
)