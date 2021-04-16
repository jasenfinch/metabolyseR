#' Calculate feature relative standard deviations
#' @rdname rsd
#' @description Calculate relative standard deviation (RSD) percentage values for each 
#' feature per class for a given sample information column.
#' @param x S4 object of class `AnalysisData`
#' @param cls sample information column to use for class structure
#' @return A tibble containing the computed RSD values.
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' rsd(d,cls = 'day')
#' @export

setGeneric("rsd", function(x,cls = 'class') {
  standardGeneric("rsd")
})

#' @rdname rsd

setMethod('rsd',signature = 'AnalysisData',
          function(x,cls = 'class'){
            vars <- 'Class'
            names(vars) <- cls
            
            x %>%
              dat() %>%
              mutate(Class = clsExtract(x,cls)) %>%
              gather(Feature,Intensity,-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Mean = mean(Intensity),
                        SD = sd(Intensity),
                        RSD = SD/Mean * 100,.groups = 'drop') %>%
              rename(!!vars)
          })
