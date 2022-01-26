#' Modelling accessor methods
#' @rdname modelling-accessors
#' @description Methods for accessing modelling results.
#' @param x S4 object of class `AnalysisData`,`RandomForest`, `Univariate`, `Analysis` or a list.
#' @param cls sample information column to use
#' @param metric importance metric for which to retrieve explanatory features
#' @param threshold threshold below which explanatory features are extracted
#' @param ... arguments to parse to method for specific class
#' @section Methods:
#' * `binaryComparisons`: Return a vector of all possible binary comparisons for a given sample information column.
#' * `type`: Return the type of random forest analysis.
#' * `response`: Return the response variable name used for a random forest analysis.
#' * `metrics`: Retrieve the model performance metrics for a random forest analysis
#' * `importanceMetrics`: Retrieve the available feature importance metrics for a random forest analysis.
#' * `importance`: Retrieve feature importance results.
#' * `proximity`: Retrieve the random forest sample proximities.
#' * `explanatoryFeatures`: Retrieve explanatory features.
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' ## Return possible binary comparisons for the 'day' column
#' binaryComparisons(d,cls = 'day')
#' 
#' ## Perform random forest analysis
#' rf_analysis <- randomForest(d,cls = 'day')
#' 
#' ## Return the type of random forest
#' type(rf_analysis)
#' 
#' ## Return the response variable name used
#' response(rf_analysis)
#' 
#' ## Retrieve the model performance metrics
#' metrics(rf_analysis)
#' 
#' ## Show the available feature importance metrics
#' importanceMetrics(rf_analysis)
#' 
#' ## Retrieve the feature importance results
#' importance(rf_analysis)
#' 
#' ## Retrieve the sample proximities
#' proximity(rf_analysis)
#' 
#' ## Retrieve the explanatory features
#' explanatoryFeatures(rf_analysis,metric = 'FalsePositiveRate',threshold = 0.05)
#' @export

setGeneric('binaryComparisons',function(x,cls = 'class')
  standardGeneric('binaryComparisons'))

#' @rdname modelling-accessors
#' @importFrom utils combn

setMethod('binaryComparisons',signature = 'AnalysisData',
          function(x,cls = 'class'){
            x %>%
              clsExtract(cls) %>%
              as.character() %>%
              unique() %>%
              sort() %>%
              combn(2) %>%
              apply(2,str_c,collapse = '~')
          }
)

#' @rdname modelling-accessors
#' @export

setGeneric("type", function(x)
  standardGeneric("type"))

#' @rdname modelling-accessors

setMethod('type',signature = 'RandomForest',function(x){
  x@type
})

#' @rdname modelling-accessors
#' @export

setGeneric("response", function(x) 
  standardGeneric("response")
)

#' @rdname modelling-accessors

setMethod('response',signature = 'RandomForest',function(x){
  x@response
})

#' @rdname modelling-accessors
#' @export

setGeneric("metrics", function(x) 
  standardGeneric("metrics")
)

#' @rdname modelling-accessors

setMethod('metrics',signature = 'RandomForest',
          function(x){
            x@results$measures
          }
)

#' @rdname modelling-accessors

setMethod('metrics',signature = 'list',
          function(x){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest')) {
              message(
                str_c('All objects contained within supplied list ',
                      'that are not of class RandomForest will be ignored.'))
            }
            
            x <- x[object_classes == 'RandomForest']
            
            if (length(x) > 0) {
              x %>%
                map(metrics) %>%
                bind_rows()  
            } else {
              tibble()
            }
            
          })

#' @rdname modelling-accessors

setMethod('metrics',signature = 'Analysis',
          function(x){
            x %>% 
              analysisResults('modelling') %>% 
              metrics()
          })

#' @rdname modelling-accessors
#' @export

setGeneric("importanceMetrics", function(x) 
  standardGeneric("importanceMetrics")
)

#' @rdname modelling-accessors

setMethod('importanceMetrics',signature = 'RandomForest',function(x){
  x %>%
    importance() %>%
    .$Metric %>%
    unique() %>%
    sort()
})

#' @rdname modelling-accessors
#' @export

setGeneric("importance", function(x) 
  standardGeneric("importance")
)

#' @rdname modelling-accessors

setMethod('importance',signature = 'RandomForest',
          function(x){
            x@results$importances %>%
              ungroup()
          }
)

#' @rdname modelling-accessors

setMethod('importance',signature = 'Univariate',
          function(x){
            x@results %>%
              ungroup()
          }
)

#' @rdname modelling-accessors

setMethod('importance',signature = 'list',
          function(x){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest' | 
                            object_classes == 'Univariate')) {
              stop(
                str_c('All objects contained within supplied list ',
                      'should be of class RandomForest or Univariate'),
                call. = FALSE)
            }
            
            x %>%
              map(importance) %>%
              bind_rows(.id = 'Method')
          })

#' @rdname modelling-accessors

setMethod('importance',signature = 'Analysis',
          function(x){
            x %>% 
              analysisResults(element = 'modelling') %>% 
              importance()
          })

#' @rdname modelling-accessors
#' @export

setGeneric("proximity", function(x) 
  standardGeneric("proximity")
)

#' @rdname modelling-accessors

setMethod('proximity',signature = 'RandomForest',
          function(x){
            x@proximities %>% 
              group_by(Response,Comparison,Sample1,Sample2) %>% 
              summarise(Proximity = mean(Proximity),
                        .groups = 'drop')
          }
)

#' @rdname modelling-accessors

setMethod('proximity',signature = 'list',
          function(x){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest')) {
              message(
                str_c('All objects contained within supplied list ',
                      'that are not of class RandomForest will be ignored.'))
            }
            
            x <- x[object_classes == 'RandomForest']
            
            if (length(x) > 0) {
              x %>%
                map(proximity) %>%
                bind_rows()  
            } else {
              tibble()
            }
            
          })

#' @rdname modelling-accessors
#' @export

setGeneric('explanatoryFeatures', function(x,...) 
  standardGeneric("explanatoryFeatures")
)

#' @rdname modelling-accessors
#' @importFrom dplyr arrange

setMethod('explanatoryFeatures',signature = 'Univariate',
          function(x,threshold = 0.05){
            importance(x) %>%
              filter(adjusted.p.value < threshold) %>% 
              arrange(adjusted.p.value)
          }
) 

#' @rdname modelling-accessors

setMethod('explanatoryFeatures',signature = 'RandomForest',
          function(x,metric = 'FalsePositiveRate', threshold = 0.05){
            
            typ <- type(x)
            
            if (typ %in% c('unsupervised','classification')) {
              explan <- explanatoryFeaturesClassification(x,metric,threshold)
            }
            
            if (typ == 'regression') {
              explan <- explanatoryFeaturesRegression(x,metric,threshold)
            }
            
            return(explan)
          }
)

#' @rdname modelling-accessors

setMethod('explanatoryFeatures',signature = 'list',
          function(x,...){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest' | 
                            object_classes == 'Univariate')) {
              stop(str_c('All objects contained within supplied ',
                         'list should be of class RandomForest or Univariate'),
                   call. = FALSE)
            }
            
            x %>%
              map(explanatoryFeatures,...) %>%
              bind_rows(.id = 'Method')
          })

#' @rdname modelling-accessors

setMethod('explanatoryFeatures',signature = 'Analysis',
          function(x,...){
            x %>% 
              analysisResults(element = 'modelling') %>% 
              explanatoryFeatures(...)
          })
