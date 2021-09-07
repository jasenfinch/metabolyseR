#' Plot sample total ion counts
#' @rdname plotTIC
#' @description Plot total ion counts of sample data.
#' @param analysis S4 object of class `AnalysisData` or `Analysis`
#' @param by information column to plot against
#' @param colour information column to provide colour labels
#' @param type `raw` or `pre-treated` sample data
#' @param ... arguments to pass to the appropriate method
#' @importFrom tibble rowid_to_column
#' @importFrom dplyr group_by summarise
#' @importFrom ggplot2 scale_fill_manual geom_boxplot
#' @importFrom stringr str_sub
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact)
#' 
#' ## Plot sample TIVs
#' plotTIC(d,by = 'injorder',colour = 'day')
#' 
#' plotTIC(d,by = 'day',colour = 'day')
#' @export

setGeneric('plotTIC', 
           function(analysis, 
                    by = 'injOrder', 
                    colour = 'block', 
                    ...)
             standardGeneric('plotTIC'))

#' @rdname plotTIC

setMethod('plotTIC',signature = 'AnalysisData',
          function(analysis, by = 'injOrder', colour = 'block') {
            d <- dat(analysis)
            info <- sinfo(analysis)
            
            index <- info %>%
              select(all_of(c(by,colour)))
            
            d <- d %>%
              bind_cols(index) %>%
              mutate(!!colour := factor(!!sym(colour))) %>%
              rowid_to_column(var = 'ID') %>%
              gather('Feature','Intensity',-ID,-all_of(c(by,colour)))
            
            if (by != colour) {
              d <- d %>%
                group_by(ID,!!sym(by),!!sym(colour))
            } else {
              d <- d %>%
                group_by(ID,!!sym(by))
                
            }
            
            d <- d %>%
              summarise(TIC = sum(Intensity))
            
            classCheck <- d %>%
              group_by(!!sym(by)) %>%
              summarise(Frequency = n())
            
            colourFreq <- analysis %>%
              clsExtract(cls = colour) %>%
              unique() %>%
              length()
            
            pl <- d %>%
              ggplot(aes(x = !!sym(by),y = TIC)) +
              theme_bw() +
              xlab(by)
            
            if (TRUE %in% (classCheck$Frequency > 1)) {
              pl <- pl + 
                geom_boxplot(aes(group = !!sym(by)),
                             outlier.shape = NA,
                             colour = 'black') +
                geom_point(aes(fill = !!sym(colour)),
                           shape = 21,
                           position = 'jitter') +
                guides(fill = 'none')
            } else {
              pl <- pl + geom_point(aes(fill = !!sym(colour)),
                                    shape = 21,
                                    size = 3)
            }
            
            if (colourFreq <= 12) {
              pl <- pl + scale_fill_ptol()
            } else {
              if (colourFreq %% 12 == 0) {
                pal <- rep(ptol_pal()(12),nrow(classCheck) / 12)
              } else {
                pal <- c(rep(ptol_pal()(12),
                             floor(nrow(classCheck) / 12)),
                         ptol_pal()(12)[1:(nrow(classCheck) %% 12)])
              }
              pl <- pl + scale_fill_manual(values = pal,name = colour)
            }
            
            pl + 
              theme(legend.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line())
          }
)

#' @rdname plotTIC

setMethod('plotTIC',signature = 'Analysis',
          function(analysis, 
                   by = 'injOrder', 
                   colour = 'block', 
                   type = c('raw','pre-treated')) {
            
            type <- match.arg(type,
                              choices = c('raw','pre-treated'))
            
            if (type == 'raw'){
              ty <- get('raw')  
            } else {
              ty <- get('preTreated')
            }
            
            
            ty(analysis) %>%
              plotTIC(by = by,colour = colour)
          }
)
