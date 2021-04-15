#' Scaling, transformation and normalisation methods
#' @rdname transform
#' @family Pre-treatment
#' @description Methods for data pre-treatment scaling, transformation and normalisation.
#' @param d S4 object of class AnalysisData 
#' @section Methods:
#' * `transformArcSine`: Arc-Sine transformation.
#' * `transformAuto`: Auto scaling.
#' * `transformCenter`: Mean centring.
#' * `transformLevel`: Level scaling.
#' * `transformLn`: Natural logarithmic transformation.
#' * `transformLog10`: Logarithmic transformation.
#' * `transformPareto`: Pareto scaling.
#' * `transformRange`: Range scaling. Also known as min-max scaling.
#' * `transformSQRT`: Square root transformation.
#' * `transformTICnorm`: Total ion count normalisation.
#' * `transformVast`: Vast scaling.

setMethod('transformArcSine',signature = 'AnalysisData',
          function(d){
            dat(d) <- asinh(d %>% dat())
            return(d)
          }
)

#' @rdname transform

setMethod('transformAuto',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / sd(.,na.rm = TRUE)})
            return(d)
          }
)

#' @rdname transform

setMethod('transformCenter',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. - mean(.,na.rm = TRUE)})
            return(d)
          }
)

#' @rdname transform

setMethod('transformLevel',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / mean(.,na.rm = TRUE)})
            return(d)
          }
)

#' @rdname transform

setMethod('transformLn',signature = 'AnalysisData',
          function(d, add = 1){
            dat(d) <- log(d %>% dat() + add) %>%
              as_tibble()
            return(d)
          }
)

#' @rdname transform

setMethod('transformLog10',signature = 'AnalysisData',
          function(d, add = 1){
            dat(d) <- log10(d %>% dat() + add) %>%
              as_tibble()
            return(d)
          }
)

#' @rdname transform

setMethod('transformPareto',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(
              d %>% 
                dat(),~{. / mean(.,na.rm = TRUE)/
                    sqrt(sd(.,na.rm = TRUE))})
            return(d)
          }
)

#' @rdname transform

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

#' @rdname transform

setMethod('transformSQRT',signature = 'AnalysisData',
          function(d){
            dat(d) <- sqrt(d %>% dat())
            return(d)
          }
)

#' @rdname transform

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

#' @rdname transform

setMethod('transformVast',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(
              d %>% 
                dat(),~{. * mean(.,na.rm = TRUE)/
                    sd(.,na.rm = TRUE)^2})
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
