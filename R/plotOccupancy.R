#' Plot class occupancy distributions
#' @rdname plotOccupancy
#' @description Plot class occupancy distributions.
#' @param x S4 object of class `AnalysisData` or `Analysis`
#' @param cls sample information column to use for class labels
#' @param type `raw` or `preTreated` data to plot
#' @param ... arguments to pass to the appropriate method
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact)
#' 
#' ## Plot class occupancy distributions
#' plotOccupancy(d,cls = 'day')
#' @export

setGeneric('plotOccupancy',function(x,cls = 'class', ...)
  standardGeneric('plotOccupancy'))

#' @rdname plotOccupancy
#' @importFrom ggplot2 element_line

setMethod('plotOccupancy',signature = 'AnalysisData',
          function(x,cls = 'class'){
            
            if (clsExtract(x,cls) %>% is.numeric()) {
              stop(
                "Argument 'cls' should be either a factor or character",
                call. = FALSE)  
            }
            
            occ <- occupancy(x,cls = cls)
            
            d <- ggplot(occ,
                        aes_string(x = 'Occupancy',
                                   group = cls,
                                   colour = cls)) +
              geom_density() +
              theme_bw() +
              labs(title = 'Density distrubution',
                   y = 'Density') +
              theme(plot.title = element_text(face = 'bold',
                                              hjust = 0.5),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom',
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line())
            
            cs <- occ %>%
              group_by_at(c(cls,'Occupancy')) %>%
              summarise(sum = n()) %>%
              mutate(cs = cumsum(sum))
          
            csDist <- ggplot(cs,
                             aes_string(x = 'Occupancy',
                                        y = 'cs',
                                        colour = cls)) + 
              geom_line() + 
              theme_bw() +
              labs(title = 'Cumulative distribution',
                   y = 'Cumulative frequency') +
              theme(plot.title = element_text(face = 'bold',
                                              hjust = 0.5),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom',
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line())
            
            if (length(clsExtract(x,cls) %>% unique()) < 12) {
              d <- d +
                scale_colour_ptol()
              
              csDist <- csDist +
                scale_colour_ptol()
            }
            
            pl <- d + csDist
            
            return(pl)
          })

#' @rdname plotOccupancy

setMethod('plotOccupancy',signature = 'Analysis',
          function(x, cls = 'class', type = 'raw'){
            ty <- get(type)
            
            x %>%
              ty() %>%
              plotOccupancy(cls = cls)
          })
