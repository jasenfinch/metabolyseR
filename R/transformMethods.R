#' transformCenter
#' @rdname transformCenter
#' @description Mean center sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformCenter',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. - mean(.,na.rm = T)})
            return(d)
          }
)

#' transformAuto
#' @rdname transformAuto
#' @description Auto scaling of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformAuto',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / sd(.,na.rm = T)})
            return(d)
          }
)

#' transformRange
#' @rdname transformRange
#' @description Range scaling of sample data. Also known as min-max scaling.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformRange',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{(. - min(.,na.rm = T)) / (max(.,na.rm = T) - min(.,na.rm = T))})
            return(d)
          }
)

#' transformPareto
#' @rdname transformPareto
#' @description Pareto scaling of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformPareto',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / mean(.,na.rm = T)/sqrt(sd(.,na.rm = T))})
            return(d)
          }
)

#' transformVast
#' @rdname transformVast
#' @description Vast scaling of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformVast',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. * mean(.,na.rm = T)/sd(.,na.rm = T)^2})
            return(d)
          }
)

#' transformLevel
#' @rdname transformLevel
#' @description Level scaling of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformLevel',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / mean(.,na.rm = T)})
            return(d)
          }
)

#' transformLn
#' @rdname transformLn
#' @description Natural logarithmic transformation of sample data.
#' @param d S4 object of class AnalysisData 
#' @param add value to add prior to transformation
#' @export

setMethod('transformLn',signature = 'AnalysisData',
          function(d, add = 1){
            dat(d) <- log(d %>% dat() + add) %>%
              as_tibble()
            return(d)
          }
)

#' transformLog10
#' @rdname transformLog10
#' @description Logarithmic transformation of sample data.
#' @param d S4 object of class AnalysisData 
#' @param add value to add prior to transformation
#' @export

setMethod('transformLog10',signature = 'AnalysisData',
          function(d, add = 1){
            dat(d) <- log10(d %>% dat() + add) %>%
              as_tibble()
            return(d)
          }
)

#' transformSQRT
#' @rdname transformSQRT
#' @description Square root transformation of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformSQRT',signature = 'AnalysisData',
          function(d){
            dat(d) <- sqrt(d %>% dat())
            return(d)
          }
)

#' transformArcSine
#' @rdname transformArcSine
#' @description Arc-sine transformation of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformArcSine',signature = 'AnalysisData',
          function(d){
            dat(d) <- asinh(d %>% dat())
            return(d)
          }
)

#' transformTICnorm
#' @rdname transformTICnorm
#' @description Total ion count normalisation of sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformTICnorm',signature = 'AnalysisData',
          function(d){
            dat(d) <- d %>% 
              dat() %>%
              split(1:nrow(.)) %>%
              map(~{. / sum(.)}) %>%
              bind_rows() %>%
              as_tibble()
            return(d)
          }
)

transformMethods <- function(method = NULL, description = F){
  
  methods <- list(
    
    center = transformCenter,
    auto = transformAuto,
    range = transformRange,
    pareto = transformPareto,
    vast = transformVast,
    level = transformLevel,
    ln = transformLn,
    log10 = transformLog10,
    sqrt = transformSQRT,
    asinh = transformArcSine,
    TICnorm = transformTICnorm
  )
  
  descriptions = list(
    center = list(description = 'Mean centering',
                  arguments = c(`''` = '')),
    auto = list(description = 'Auto scaling',
                arguments = c(`''` = '')),
    range = list(description = 'Range scaling',
                 arguments = c(`''` = '')),
    pareto = list(description = 'Pareto scaling',
                  arguments = c(`''` = '')),
    vast = list(description = 'Vast scaling',
                arguments = c(`''` = '')),
    level = list(description = 'Level scaling',
                 arguments = c(`''` = '')),
    ln = list(description = 'Natural log scaling',
               arguments = c(add = 'value to add prior to transformation')),
    log10 = list(description = 'Log10 scaling',
                 arguments = c(add = 'value to add prior to transformation')),
    sqrt = list(description = 'Square root scaling',
                arguments = c(`''` = '')),
    asinh = list(description = 'Arc-sine scaling',
                 arguments = c(`''` = '')),
    TICnorm = list(description = 'Total ion count normalisation',
                   arguments = c(`''` = ''))
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      method <- descriptions[[method]]
    }
  }
  return(method)
}
