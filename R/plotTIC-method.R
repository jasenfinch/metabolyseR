#' plotTIC
#' @rdname plotTIC
#' @description Plot total ion counts of sample raw data.
#' @param analysis object of class Analysis containing analysis results
#' @param by info column to plot against
#' @param colour info column to provide colour labels
#' @param modes split modes if present
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 scale_colour_manual geom_boxplot
#' @importFrom stringr str_sub
#' @examples 
#' 
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters(c('preTreat'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)
#' plotTIC(analysis,by = 'injorder',colour = 'day',modes = FALSE) 
#' @export

setMethod('plotTIC',signature = 'Analysis',
          function(analysis, by = 'injOrder', colour = 'block', modes = T) {
            dat <- rawData(analysis)
            info <- rawInfo(analysis)
            
            index <- info %>%
              select(Index = by,Colour = colour)
            
            dat <- dat %>%
              bind_cols(index) %>%
              mutate(Colour = factor(Colour)) %>%
              rowid_to_column(var = 'ID') %>%
              gather('Feature','Intensity',-ID,-Index,-Colour) 
            
            if (modes == T) {
              dat <- dat %>%
                mutate(Mode = str_sub(Feature,1,1)) %>%
                group_by(ID,Mode,Index,Colour)
            } else {
              dat <- dat %>%
                group_by(ID,Index,Colour)  
            }
            dat <- dat %>%
              summarise(TIC = sum(Intensity))
            
            classCheck <- dat %>%
              group_by(Index) %>%
              summarise(Frequency = n())
            
            pl <- dat %>%
              ggplot(aes(x = Index,y = TIC,colour = Colour)) +
              theme_bw() +
              xlab(by)
            
            if (T %in% (classCheck$Frequency > 1)) {
              pl <- pl + 
                geom_boxplot(aes(group = Index),outlier.shape = NA,colour = 'black') +
                geom_point(position = 'jitter')
            } else {
              pl <- pl + geom_point()
            }
            
            if (nrow(classCheck) <= 12) {
              pl <- pl + scale_colour_ptol(name = colour)
            } else {
              if (nrow(classCheck) %% 12 == 0) {
                pal <- rep(ptol_pal()(12),nrow(classCheck) / 12)
              } else {
                pal <- c(rep(ptol_pal()(12),floor(nrow(classCheck) / 12)),ptol_pal()(12)[1:(nrow(classCheck) %% 12)])
              }
              pl <- pl + scale_colour_manual(values = pal,name = colour)
            }
            
            if (modes == T) {
              pl <- pl + facet_wrap(~Mode)
            }
            
            pl
          }
)