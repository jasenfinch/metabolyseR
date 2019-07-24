#' aggregateSum
#' @rdname aggregateSum
#' @description Sum aggregation of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @importFrom dplyr tbl_df arrange_
#' @export

setMethod('aggregateSum',signature = 'AnalysisData',
          function(dat,cls = 'class'){
            dat@data <- dat %>%
              dat() %>%
              bind_cols(select(dat %>% info(),Class = cls)) %>%
              gather('Feature','Intensity',-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = sum(Intensity)) %>%
              tbl_df() %>%
              spread(Feature,Intensity) %>%
              select(-Class)
            
            dat@info <- dat %>%
              info() %>% 
              select(cls) %>%
              unique() %>%
              arrange_(cls)
            
            return(dat)
          }
)

#' aggregateMean
#' @rdname aggregateMean
#' @description Mean aggregation of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @export

setMethod('aggregateMean',signature = 'AnalysisData',
          function(dat,cls = 'class'){
            dat@data <- dat %>%
              dat() %>%
              bind_cols(select(dat %>% info(),Class = cls)) %>%
              gather('Feature','Intensity',-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = mean(Intensity)) %>%
              tbl_df() %>%
              spread(Feature,Intensity) %>%
              select(-Class)
            
            dat@info <- dat %>%
              info() %>%
              select(cls) %>%
              unique() %>%
              arrange_(cls)
            
            return(dat)
          }
)

#' aggregateMedian
#' @rdname aggregateMedian
#' @description Median aggregation of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @export

setMethod('aggregateMedian',signature = 'AnalysisData',
          function(dat, cls = 'class'){
            dat@data <- dat %>%
              dat() %>%
              bind_cols(select(dat %>% info(),Class = cls)) %>%
              gather('Feature','Intensity',-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = median(Intensity)) %>%
              tbl_df() %>%
              spread(Feature,Intensity) %>%
              select(-Class)
            
            dat@info <- dat %>%
              info() %>% 
              select(cls) %>%
              unique() %>%
              arrange_(cls)
            
            return(dat)
          }
)

aggregateMethods <- function(method = NULL, description = F){
  
  methods <- list(
    sum = aggregateSum,
    mean = aggregateMean,
    median = aggregateMedian
  )
  
  descriptions = list(
    sum = list(description = 'sum aggregate',
               arguments = c(cls = 'info column to use for aggregation index')),
    mean = list(description = 'mean aggregate',
                arguments = c(cls = 'info column to use for aggregation index')),
    median = list(description = 'median aggregate',
                  arguments = c(cls = 'info column to use for aggregation index'))
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