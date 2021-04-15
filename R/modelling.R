#' binaryComparisons
#' @rdname binaryComparisons
#' @description Return a vector of possible binary comparisons for a 
#' given sample information column.
#' @param x S4 object of class AnalysisData.
#' @param cls sample information column to use
#' @importFrom utils combn
#' @export

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

#' modellingMethods
#' @description Return names of available modelling methods.
#' @export

modellingMethods <- function(){
  getModellingMethods() %>%
    names()
}

getModellingMethods <- function(method = NULL){
  
  methods <- list(
    anova = anova,
    ttest = ttest,
    linearRegression = linearRegression,
    randomForest = randomForest
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    method <- methods[[method]]
  }
  
  return(method)
}

setGeneric("modelling", function(x) {
  standardGeneric("modelling")
})

setMethod('modelling',signature = 'Analysis',
          function(x){
            verbose <- x@log$verbose
            if (verbose == TRUE) {
              startTime <- proc.time()
              message(
                blue('Modelling '),
                cli::symbol$continue,
                '\r',
                appendLF = FALSE) 
            }
            params <- x %>%
              parameters() %>%
              parameters('modelling')
            
            res <- params %>%
              names() %>%
              map(~{
                i <- .
                method <- getModellingMethods(i)
                
                if (x %>% 
                    dat(type = 'pre-treated') %>% 
                    nrow() > 0) {
                  d <- preTreated(x)
                } else {
                  d <- raw(x)
                }
                
                newPars <- formals(method) %>%
                  as.list()
                newPars[names(params[[i]])] <- params[[i]]
                newPars[[1]] <- d
                
                do.call(method,newPars)
              }) %>%
              set_names(names(params))
            
            x@modelling <- res
            x@log$modelling <- date()
            
            if (verbose == TRUE) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              message(
                blue('\rModelling '),
                '\t',
                green(cli::symbol$tick),
                ' ',
                elapsed)
            }
            return(x)
          }
)

#' type
#' @rdname type
#' @description Return the random forest analysis type.
#' @param x S4 object of class RandomForest
#' @export

setMethod('type',signature = 'RandomForest',function(x){
  x@type
})

#' response
#' @rdname response
#' @description Return the response variable name from a random forest analysis.
#' @param x S4 object of class RandomForest
#' @export

setMethod('response',signature = 'RandomForest',function(x){
  x@response
})

#' importanceMetrics
#' @rdname importanceMetrics
#' @description Return available importance measures from an object 
#' of class RandomForest.
#' @param x S4 object of class RandomForest
#' @export

setMethod('importanceMetrics',signature = 'RandomForest',function(x){
  x %>%
    importance() %>%
    .$Metric %>%
    unique() %>%
    sort()
})

#' explanatoryFeatures
#' @rdname explanatoryFeatures
#' @description Extract explanatory features from modelling results.
#' @param x S4 object of class RandomForest or Univariate
#' @param metric importance metric on which to retrieve explanatory feautres
#' @param threshold threshold below which explanatory features are extracted
#' @param ... arguments to parse to method for specific class
#' @importFrom dplyr arrange
#' @export

setMethod('explanatoryFeatures',signature = 'Univariate',
          function(x,threshold = 0.05,...){
            importance(x) %>%
              filter(adjusted.p.value < threshold) %>% 
              arrange(adjusted.p.value)
          }
) 

#' @rdname explanatoryFeatures
#' @export

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

explanatoryFeaturesClassification <- function(x,metric,threshold){
  
  imp <- x %>%
    importance()
  
  metrics <- importanceMetrics(x)
  
  if (!(metric %in% metrics)) {
    
    metrics <- str_c('"',metrics,'"')
    
    stop(
      'Argument "metric" should be one of ',
      str_c(metrics,collapse = ', '),
      call. = FALSE)
  }
  
  explan <- imp %>%
    filter(Metric == metric)
  
  if (metric == 'FalsePositiveRate') {
    explan <- explan %>%
      filter(Value < threshold) %>% 
      arrange(Value)
  } else {
    explan <- explan %>%
      filter(Value > threshold) %>% 
      arrange(desc(Value))
  }
  
  return(explan)
}

explanatoryFeaturesRegression <- function(x,metric,threshold){
  
  imp <- x %>%
    importance()
  
  metrics <- importanceMetrics(x)
  
  if (!(metric %in% metrics)) {
    
    metrics <- str_c('"',metrics,'"')
    
    stop(
      'Argument "metric" should be one of ',
      str_c(metrics,collapse = ', '),
      call. = FALSE)
  }
  
  explan <- imp %>%
    filter(Metric == metric) %>%
    filter(Value > threshold) %>% 
    arrange(desc(Value))
  
  return(explan)
}

#' @rdname explanatoryFeatures
#' @export

setMethod('explanatoryFeatures',signature = 'list',
          function(x,threshold = 0.05, ...){
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

#' @rdname explanatoryFeatures
#' @export

setMethod('explanatoryFeatures',signature = 'Analysis',
          function(x,threshold = 0.05, ...){
            x %>% 
              analysisResults(element = 'modelling') %>% 
              explanatoryFeatures(...)
          })

