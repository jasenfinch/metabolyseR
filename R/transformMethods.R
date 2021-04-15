#' transformCenter
#' @rdname transformCenter
#' @description Mean center sample data.
#' @param d S4 object of class AnalysisData 
#' @export

setMethod('transformCenter',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. - mean(.,na.rm = TRUE)})
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
            dat(d) <- map_df(d %>% dat(),~{. / sd(.,na.rm = TRUE)})
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
            dat(d) <- map_df(
              d %>% 
                dat(),
              ~{(. - min(.,na.rm = TRUE)) / 
                  (max(.,na.rm = TRUE) - min(.,na.rm = TRUE))})
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
            dat(d) <- map_df(
              d %>% 
                dat(),~{. / mean(.,na.rm = TRUE)/
                    sqrt(sd(.,na.rm = TRUE))})
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
            dat(d) <- map_df(
              d %>% 
                dat(),~{. * mean(.,na.rm = TRUE)/
                    sd(.,na.rm = TRUE)^2})
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
            dat(d) <- map_df(d %>% dat(),~{. / mean(.,na.rm = TRUE)})
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
              base::split(seq_len(nrow(.))) %>%
              map(~{. / sum(.)}) %>%
              bind_rows() %>%
              as_tibble()
            return(d)
          }
)

transformMethods <- function(method = NULL){
  
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
  
  if (is.null(method)) {
    method <- methods
  } else {
    if (!(method %in% names(methods))) {
      stop(str_c("Transform method '",
                 method,
                 "' not recognised. Available methods include: ",
                 str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
    }
    method <- methods[[method]]
  }
  
  return(method)
}
