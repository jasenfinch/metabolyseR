#' rsd
#' @rdname rsd
#' @description Calculate relative standard deviation (RSD) percent values for each 
#' feature per class for a given info column.
#' @param x S4 object of class AnalysisData
#' @param cls info column to use for class structure
#' @export

setMethod('rsd',signature = 'AnalysisData',
          function(x,cls = 'class'){
            vars <- 'Class'
            names(vars) <- cls
            
            x %>%
              dat() %>%
              mutate(Class = clsExtract(x,cls)) %>%
              gather(Feature,Intensity,-Class) %>%
              group_by(Class,Feature) %>%
              summarise(RSD = sd(Intensity)/mean(Intensity) * 100,.groups = 'drop') %>%
              rename(!!vars)
          })
