#' Scaling, transformation and normalisation methods
#' @rdname transform
#' @description Methods for data scaling, transformation and normalisation.
#' @param d S4 object of class `AnalysisData` 
#' @param add value to add prior to transformation
#' @return An S4 object of class `AnalysisData` containing the transformed data.
#' @section Methods:
#' * `transformArcSine`: Arc-sine transformation.
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
#' @examples 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' ## Arc-sine transformation
#' d %>% transformArcSine()
#' 
#' ## Auto scaling
#' d %>% transformAuto()
#' 
#' ## Mean centring
#' d %>% transformCenter()
#' 
#' ## Level scaling
#' d %>% transformLevel()
#' 
#' ## Natural logarithmic transformation
#' d %>% transformLn()
#' 
#' ## Logarithmic transformation
#' d %>% transformLog10
#' 
#' ## Pareto scaling
#' d %>% transformPareto
#' 
#' ## Range scaling
#' d %>% transformRange()
#' 
#' ## Square root scaling
#' d %>% transformSQRT()
#' 
#' ## Total ion count nromalisation
#' d %>% transformTICnorm()
#' 
#' ## Vast scaling
#' d %>% transformVast()

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
