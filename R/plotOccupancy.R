#' plotOccupancy
#' @rdname plotOccupancy
#' @description Plot occupancy distributions.
#' @param x S4 object of class AnalysisData or Analysis
#' @param cls info column to use for class labels
#' @param type \code{raw} or \code{preTreated} data to plot
#' @param ... arguments to pass to the appropriate method
#' @export

setMethod('plotOccupancy',signature = 'AnalysisData',
          function(x,cls = 'class'){
            
            if (clsExtract(x,cls) %>% is.numeric()) {
              stop("Argument 'cls' should be either a factor or character",call. = FALSE)  
            }
            
            occ <- occupancy(x,cls = cls)
            
            d <- ggplot(occ,aes_string(x = 'Occupancy',group = cls,colour = cls)) +
              geom_density() +
              theme_bw() +
              labs(title = 'Density distrubution',
                   y = 'Density') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom')
            
            cs <- occ %>%
              group_by_at(c(cls,'Occupancy')) %>%
              summarise(sum = n()) %>%
              mutate(cs = cumsum(sum))
          
            csDist <- ggplot(cs,aes_string(x = 'Occupancy',y = 'cs',colour = cls)) + 
              geom_line() + 
              theme_bw() +
              labs(title = 'Cumulative distribution',
                   y = 'Cumulative frequency') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom')
            
            pl <- d + csDist
            
            return(pl)
          })

#' @rdname plotOccupancy
#' @export

setMethod('plotOccupancy',signature = 'Analysis',
          function(x, cls = 'class', type = 'raw'){
            ty <- get(type)
            
            x %>%
              ty() %>%
              plotOccupancy(cls = cls)
          })