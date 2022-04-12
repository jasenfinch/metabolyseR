#' Sample aggregation
#' @rdname aggregate
#' @description Aggregation of sample features based on a grouping variable.
#' @param d S4 object of class `AnalysisData`
#' @param cls info columns across which to aggregate the data
#' @return  An S4 object of class `AnalysisData` containing the aggregated data.
#' @details 
#' Sample aggregation allows the electronic pooling of sample features based on a grouping variable. 
#' This is useful in situations such as the presence of technical replicates that can be aggregated to reduce the effects of pseudo replication.
#' @section Methods:
#' * `aggregateMean`: Aggregate sample features to the group mean.
#' * `aggregateMedian`: Aggregate sample features to the group median.
#' * `aggregateSum`: Aggregate sample features to the group total.
#' @examples 
#' ## Each of the following examples shows the application of the aggregation method and then 
#' ## a Principle Component Analysis is plotted to show it's effect on the data structure.
#' 
#' ## Initial example data preparation
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact) %>% 
#'  occupancyMaximum(occupancy = 2/3)
#' 
#' d %>% 
#'  plotPCA(cls = 'day')
#'  
#' ## Mean aggregation
#' d %>% 
#'  aggregateMean(cls = c('day','class')) %>% 
#'  plotPCA(cls = 'day',ellipses = FALSE)
#'  
#' ## Median aggregation
#' d %>% 
#'  aggregateMedian(cls = c('day','class')) %>% 
#'  plotPCA(cls = 'day',ellipses = FALSE)
#'  
#' ## Sum aggregation
#' d %>% 
#'  aggregateSum(cls = c('day','class')) %>% 
#'  plotPCA(cls = 'day',ellipses = FALSE)
#' @export

setGeneric("aggregateMean", function(d,cls = 'class') 
  standardGeneric("aggregateMean")
)

#' @rdname aggregate

setMethod('aggregateMean',signature = 'AnalysisData',
          function(d,cls = 'class'){
            d <- aggregate(d,'mean',cls)
            return(d)
          }
)

#' @rdname aggregate
#' @export

setGeneric("aggregateMedian", function(d,cls = 'class') 
  standardGeneric("aggregateMedian")
)

#' @rdname aggregate

setMethod('aggregateMedian',signature = 'AnalysisData',
          function(d, cls = 'class'){
            d <- aggregate(d,'median',cls)
            return(d)
          }
)

#' @rdname aggregate
#' @export

setGeneric("aggregateSum", function(d,cls = 'class') 
  standardGeneric("aggregateSum")
)

#' @rdname aggregate
#' @importFrom dplyr arrange_all group_by_all

setMethod('aggregateSum',signature = 'AnalysisData',
          function(d,cls = 'class'){
            d <- aggregate(d,'sum',cls)
            return(d)
          }
)

aggregate <- function(d,method,cls){
  aggregateMethod <- switch(method,
                            mean = mean,
                            median = median,
                            sum = sum)
  
  sample_info <- d %>% 
    sinfo() %>% 
    select(all_of(cls))
  
  dat(d) <- d %>%
    dat() %>%
    bind_cols(sample_info) %>%
    gather('Feature','Intensity',-all_of(cls)) %>%
    group_by(across(cls),Feature) %>%
    summarise(Intensity = aggregateMethod(Intensity)) %>%
    ungroup() %>%
    spread(Feature,Intensity) %>%
    select(-all_of(cls))
  
  sinfo(d) <- sample_info %>%
    group_by_all() %>%
    summarise() %>%
    arrange_all()
  
  return(d)
  
}
