#' @importFrom utils combn

getPairwises <- function(cl){
  cl %>%
    as.character() %>%
    unique() %>%
    sort() %>%
    combn(2) %>%
    apply(2,str_c,collapse = '~')
}

#' getClusterType
#' @description Return appropriate cluster type for parallel processing based on operating system type.
#' @export

getClusterType <- function(){
  if (.Platform$OS.type == 'windows') {
    type <- 'PSOCK'
  } else {
    type <- 'FORK'
  }
  return(type)
}

#' modellingParameters
#' @description Return parameters for a given modelling method.
#' @param methods character vector of available methods. Set to NULL to print available methods.
#' @export

modellingParameters <- function(methods){
  
  availableMethods <- c('anova','ttest','linearRegression','randomForest')
  
  if (is.null(methods)) {
    cat('Available methods:\t',str_c(availableMethods,collapse = '\n\t\t\t'),sep = '')
  }
  
  if (F %in% (methods %in% availableMethods)) {
    stop(str_c('Modelling method not found! Methods should be one of: ',str_c(availableMethods,collapse = ', '),'.'))
  }
  
  methods %>%
    map(~{
      formals(.) %>%
        .[-1]
    }) %>%
    set_names(methods)
}

modellingMethods <- function(method = NULL, description = F){
  
  methods <- list(
    anova = anova,
    ttest = ttest,
    linearRegression = linearRegression,
    randomForest = randomForest
  )
  
  descriptions = list(
    anova = list(description = 'One-way ANOVA',
                 type = 'Univariate',
                 documentation = '?anova'),
    ttest = list(description = 'Welch t-test',
                 type = 'Univariate',
                 documentation = '?ttest'),
    linearRegression = list(description = 'Linear regression',
                            type = 'Univariate',
                            documentation = '?linearRegression'),
    randomForest = list(description = 'Random forest classification, regression or unsupervised',
                        type = 'Multivariate',
                        documentation = '?randomForest')
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

setMethod('modelling',signature = 'Analysis',
          function(x){
            verbose <- x@log$verbose
            if (verbose == T) {
              startTime <- proc.time()
              message(blue('Modelling '),cli::symbol$continue,'\r',appendLF = FALSE) 
            }
            params <- x@parameters@modelling
            
            res <- params %>%
              names() %>%
              map(~{
                i <- .
                method <- modellingMethods(i)
                
                if (nrow(x@preTreated@data) > 0) {
                  d <- x@preTreated
                } else {
                  d <- x@rawData
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
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              message(blue('\rModelling '),'\t',green(cli::symbol$tick),' ',elapsed)
            }
            return(x)
          }
)

#' modellingResults
#' @rdname modellingResults
#' @description Return modelling results from an Analysis object.
#' @param x S4 object of class Analysis
#' @export

setMethod('modellingResults',signature = 'Analysis',
          function(x){
            x@modelling
          }
)

#' explanatoryFeatures
#' @rdname explanatoryFeatures
#' @description Extract explanatory features from modelling results.
#' @param x S4 object of class RandomForest or Univariate
#' @param measure importance measure on which to retrieve explanatory feautres
#' @param threshold threshold below which explanatory features are extracted
#' @param ... arguments to parse to method for specific class
#' @export

setMethod('explanatoryFeatures',signature = 'Univariate',
          function(x,threshold = 0.05){
            importance(x) %>%
              filter(adjusted.p.value < threshold)
          }
) 

#' @rdname explanatoryFeatures
#' @export

setMethod('explanatoryFeatures',signature = 'RandomForest',
          function(x,measure = 'FalsePositiveRate', threshold = 0.05){
            explan <- importance(x) %>%
              filter(Measure == measure)
            
            if ('adjustedPvalue' %in% colnames(explan)) {
              explan <- explan %>%
                filter(adjustedPvalue < threshold)
            } else {
              message('Permutation results not found, using measure value instead.')
              explan <- explan %>%
                filter(Value < threshold)
            }
            
            return(explan)
          }
) 

#' @rdname explanatoryFeatures
#' @export

setMethod('explanatoryFeatures',signature = 'list',
          function(x,threshold = 0.05, ...){
            object_classes <- x %>%
              map_chr(class)
            
            if (F %in% (object_classes == 'RandomForest' | object_classes == 'Univariate')) {
              stop('All objects contained within supplied list should be of class RandomForest or Univariate',call. = FALSE)
            }
            
            x %>%
              map(explanatoryFeatures) %>%
              bind_rows()
          })

