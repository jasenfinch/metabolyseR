#' aggregateSum
#' @rdname aggregateSum
#' @description Sum aggregation of sample data.
#' @param d S4 object of class Data
#' @param cls info column to use for class data
#' @importFrom dplyr arrange_all group_by_all
#' @export

setMethod('aggregateSum',signature = 'AnalysisData',
          function(d,cls = 'class'){
            dat(d) <- d %>%
              dat() %>%
              bind_cols(select(d %>% sinfo(),Class = cls)) %>%
              gather('Feature','Intensity',-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = sum(Intensity)) %>%
              ungroup() %>%
              spread(Feature,Intensity) %>%
              select(-Class)
            
            sinfo(d) <- d %>%
              sinfo() %>% 
              select(cls) %>%
              group_by_all() %>%
              summarise() %>%
              arrange_all()
            
            return(d)
          }
)

#' aggregateMean
#' @rdname aggregateMean
#' @description Mean aggregation of sample data.
#' @param d S4 object of class Data
#' @param cls info column to use for class data
#' @export

setMethod('aggregateMean',signature = 'AnalysisData',
          function(d,cls = 'class'){
            dat(d) <- d %>%
              dat() %>%
              bind_cols(select(d %>% sinfo(),Class = cls)) %>%
              gather('Feature','Intensity',-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = mean(Intensity)) %>%
              ungroup() %>%
              spread(Feature,Intensity) %>%
              select(-Class)
            
            sinfo(d) <- d %>%
              sinfo() %>%
              select(cls) %>%
              group_by_all() %>%
              summarise() %>%
              arrange_all()
            
            return(d)
          }
)

#' aggregateMedian
#' @rdname aggregateMedian
#' @description Median aggregation of sample data.
#' @param d S4 object of class Data
#' @param cls info column to use for class data
#' @export

setMethod('aggregateMedian',signature = 'AnalysisData',
          function(d, cls = 'class'){
            dat(d) <- d %>%
              dat() %>%
              bind_cols(select(d %>% sinfo(),Class = cls)) %>%
              gather('Feature','Intensity',-Class) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = median(Intensity)) %>%
              ungroup() %>%
              spread(Feature,Intensity) %>%
              select(-Class)
            
            sinfo(d) <- d %>%
              sinfo() %>% 
              select(cls) %>%
              group_by_all() %>%
              summarise() %>%
              arrange_all()
            
            return(d)
          }
)

aggregateMethods <- function(method = NULL, description = FALSE){
  
  methods <- list(
    sum = aggregateSum,
    mean = aggregateMean,
    median = aggregateMedian
  )
  
  descriptions <- list(
    sum = list(description = 'sum aggregate',
               arguments = c(
                 cls = 'info column to use for aggregation index')),
    mean = list(description = 'mean aggregate',
                arguments = c(
                  cls = 'info column to use for aggregation index')),
    median = list(description = 'median aggregate',
                  arguments = c(
                    cls = 'info column to use for aggregation index'))
  )
  
  if (description == FALSE) {
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Aggregate method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Aggregate method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}
