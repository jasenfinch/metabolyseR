#' Plot RSD distributions
#' @rdname plotRSD
#' @description Plot RSD distributions of raw data in quality control samples.
#' @param analysis object of class `AnalysisData` or `Analysis`
#' @param cls information column to use for class labels
#' @param type `raw` or `pre-treated` data to plot 
#' @param ... arguments to pass to the appropriate method
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @importFrom stats median
#' @importFrom ggplot2 geom_histogram geom_text geom_density
#' @importFrom patchwork plot_layout
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact)
#' 
#' ## Plot class RSD distributions
#' plotRSD(d,cls = 'day')
#' @export

setGeneric('plotRSD', 
           function(analysis, 
                    cls = 'class', 
                    ...)
             standardGeneric('plotRSD'))

#' @rdname plotRSD

setMethod('plotRSD',signature = 'AnalysisData',
          function(analysis, cls = 'class'){
            
            if (clsExtract(analysis,cls) %>% is.numeric()) {
              stop(
                "Argument 'cls' should be either a factor or character",
                call. = FALSE)  
            }
            
            x <- rsd(analysis,cls = cls)
            
            d <- ggplot(
              x,
              aes(
                x = RSD,
                colour = .data[[cls]],
                group = .data[[cls]])) +
              geom_density() +
              theme_bw() +
              labs(title = 'Density distrubution',
                   x = 'RSD (%)',
                   y = 'Density') +
              theme(plot.title = element_text(face = 'bold',
                                              hjust = 0.5),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom',
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line())
            
            cs <- x %>%
              group_by_at(c(cls,'RSD')) %>%
              summarise(sum = n()) %>%
              mutate(cs = cumsum(sum))
            
            csDist <- ggplot(
              cs,
              aes(
                x = RSD,
                y = cs,
                colour = .data[[cls]])) + 
              geom_line() + 
              theme_bw() +
              labs(title = 'Cumulative distribution',
                   x = 'RSD (%)',
                   y = 'Cumulative frequency') +
              theme(plot.title = element_text(face = 'bold',
                                              hjust = 0.5),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom',
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line())
            
            if (length(clsExtract(analysis,cls) %>% unique()) < 12) {
              d <- d +
                scale_colour_ptol()
              
              csDist <- csDist +
                scale_colour_ptol()
            }
            
            pl <- d + csDist
            
            return(pl)
          }
)

#' @rdname plotRSD

setMethod('plotRSD',signature = 'Analysis',
          function(analysis,
                   cls = 'class',
                   type = 'raw'){
            
            d <- analysisData(dat(analysis,type = type),
                              sinfo(analysis,type = type))
            
            plotRSD(d,cls = cls)
          }
)
