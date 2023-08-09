#' Scaling, transformation and normalisation methods
#' @rdname transform
#' @description Methods for data scaling, transformation and normalisation.
#' @param d S4 object of class `AnalysisData` 
#' @param add value to add prior to transformation
#' @param refactor TRUE/FALSE. Re-factor the normalised intensity values to a range consistent with the raw values by multiplying by the maximum sample TIC.
#' @return An S4 object of class `AnalysisData` containing the transformed data.
#' @details 
#' Prior to downstream analyses, metabolomics data often require transformation to fulfil the assumptions of a particular statistical/data mining technique.
#' Before applying a transformation, it is important to consider the effects that the transformation will have on the data, as this can greatly effect the outcome of further downstream analyses.
#' It is also important to consider at what stage in the pre-treatment routine a transformation is applied as this too could introduce artefacts into the data.
#' The best practice is to apply a transformation as the last in a pre-treatment routine after all other steps have been taken. 
#' There are a wide range of transformation methods available that are commonly used for the analysis of metabolomics data.
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
#' 
#' ## Each of the following examples shows the application of the transformation and then 
#' ## a Linear Discriminant Analysis is plotted to show it's effect on the data structure.
#' 
#' ## Initial example data preparation
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact) %>% 
#'  occupancyMaximum(occupancy = 2/3)
#' 
#' d %>% 
#'  plotLDA(cls = 'day')
#'  
#' 
#' ## Arc-sine transformation
#' d %>% 
#'  transformArcSine() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Auto scaling
#' d %>% 
#'  transformAuto() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Mean centring
#' d %>% 
#'  transformCenter()%>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Level scaling
#' d %>% 
#'  transformLevel() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Natural logarithmic transformation
#' d %>% 
#'  transformLn() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Logarithmic transformation
#' d %>% 
#'  transformLog10()%>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Pareto scaling
#' d %>% 
#'  transformPareto() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Range scaling
#' d %>% 
#'  transformRange() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Square root scaling
#' d %>% 
#'  transformSQRT() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Total ion count nromalisation
#' d %>% 
#'  transformTICnorm() %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Vast scaling
#' d %>% 
#'  transformVast() %>% 
#'  plotLDA(cls = 'day')
#' @export

setGeneric("transformArcSine", function(d)
  standardGeneric("transformArcSine"))

#' @rdname transform

setMethod('transformArcSine',signature = 'AnalysisData',
          function(d){
            dat(d) <- asinh(d %>% dat())
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformAuto", function(d)
  standardGeneric("transformAuto"))

#' @rdname transform

setMethod('transformAuto',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / sd(.,na.rm = TRUE)})
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformCenter", function(d)
  standardGeneric("transformCenter"))

#' @rdname transform

setMethod('transformCenter',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. - mean(.,na.rm = TRUE)})
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformLevel", function(d)
  standardGeneric("transformLevel"))

#' @rdname transform

setMethod('transformLevel',signature = 'AnalysisData',
          function(d){
            dat(d) <- map_df(d %>% dat(),~{. / mean(.,na.rm = TRUE)})
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformLn", function(d,add = 1)
  standardGeneric("transformLn"))

#' @rdname transform

setMethod('transformLn',signature = 'AnalysisData',
          function(d, add = 1){
            dat(d) <- log(d %>% dat() + add) %>%
              as_tibble()
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformLog10", function(d,add = 1)
  standardGeneric("transformLog10"))

#' @rdname transform

setMethod('transformLog10',signature = 'AnalysisData',
          function(d, add = 1){
            dat(d) <- log10(d %>% dat() + add) %>%
              as_tibble()
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformPareto", function(d)
  standardGeneric("transformPareto"))

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
#' @export

setGeneric("transformRange", function(d)
  standardGeneric("transformRange"))

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
#' @export

setGeneric("transformSQRT", function(d)
  standardGeneric("transformSQRT"))

#' @rdname transform

setMethod('transformSQRT',signature = 'AnalysisData',
          function(d){
            dat(d) <- sqrt(d %>% dat())
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformTICnorm", function(d,refactor = TRUE)
  standardGeneric("transformTICnorm"))

#' @rdname transform

setMethod('transformTICnorm',signature = 'AnalysisData',
          function(d, refactor = TRUE){
            
            raw_data <- dat(d)
            
            tics <- rowSums(raw_data)
            
            normalised_data <- raw_data %>% 
              {. / tics}
            
            if (refactor){
              normalised_data <- normalised_data * max(tics)
            }
            
            dat(d) <- normalised_data %>% 
              as_tibble()
            
            return(d)
          }
)

#' @rdname transform
#' @export

setGeneric("transformVast", function(d)
  standardGeneric("transformVast"))

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
