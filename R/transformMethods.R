#' transformCenter
#' @rdname transformCenter
#' @description Mean center sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformCenter',signature = 'AnalysisData',
          function(dat){
            dat@data <- apply(dat$Data,2,function(x){x - mean(x,na.rm = T)})
            return(dat)
          }
)

#' transformAuto
#' @rdname transformAuto
#' @description Auto scaling of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformAuto',signature = 'AnalysisData',
          function(dat){
            dat@data <- apply(dat %>% dat,2,function(x){x / sd(x,na.rm = T)})
            return(dat)
          }
)

#' transformRange
#' @rdname transformRange
#' @description Range scaling of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformRange',signature = 'AnalysisData',
          function(dat){
            dat@data <- apply(dat %>% dat(),2,function(x){x / (max(x,na.rm = T) - min(x,na.rm = T))})
            return(dat)
          }
)

#' transformPareto
#' @rdname transformPareto
#' @description Pareto scaling of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformPareto',signature = 'AnalysisData',
          function(dat){
            dat@data <- apply(dat %>% dat(),2,function(x){x / mean(x,na.rm = T)/sqrt(sd(x,na.rm = T))})
            return(dat)
          }
)

#' transformVast
#' @rdname transformVast
#' @description Vast scaling of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformVast',signature = 'AnalysisData',
          function(dat){
            dat@data <- apply(dat %>% dat(),2,function(x){x * mean(x,na.rm = T)/sd(x,na.rm = T)^2})
            return(dat)
          }
)

#' transformLevel
#' @rdname transformLevel
#' @description Level scaling of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformLevel',signature = 'AnalysisData',
          function(dat){
            dat@data <- apply(dat %>% dat(),2,function(x){x / mean(x,na.rm = T)})
            return(dat)
          }
)

#' transformLn
#' @rdname transformLn
#' @description Natural logarithmic transformation of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformLn',signature = 'AnalysisData',
          function(dat, add = 1){
            dat@data <- log(dat %>% dat() + add)
            return(dat)
          }
)

#' transformLog10
#' @rdname transformLog10
#' @description Logarithmic transformation of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformLog10',signature = 'AnalysisData',
          function(dat, add = 1){
            dat@data <- log10(dat %>% dat() + add)
            return(dat)
          }
)

#' transformSQRT
#' @rdname transformSQRT
#' @description Square root transformation of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformSQRT',signature = 'AnalysisData',
          function(dat){
            dat@data <- sqrt(dat %>% dat())
            return(dat)
          }
)

#' transformArcSine
#' @rdname transformArcSine
#' @description Arc-sine transformation of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformArcSine',signature = 'AnalysisData',
          function(dat){
            dat@data <- asinh(dat %>% dat())
            return(dat)
          }
)

#' transformTICnorm
#' @rdname transformTICnorm
#' @description Total ion count normalisation of sample data.
#' @param dat S4 object of class AnalysisData 
#' @export

setMethod('transformTICnorm',signature = 'AnalysisData',
          function(dat){
            dat@data <- dat %>% 
              dat() %>%
              split(1:nrow(.)) %>%
              map(~{. / sum(.)}) %>%
              bind_rows() %>%
              as_tibble()
            return(dat)
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
