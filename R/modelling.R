
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
  
  availableMethods <- c('ttest','linearRegression','randomForest')
  
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
    ttest = ttest,
    linearRegression = linearRegression,
    randomForest = randomForest
  )
  
  descriptions = list(
    ttest = list(description = 'Welch t-test',
                 documentation = '?ttest'),
    linearRegression = list(description = 'Linear regression',
                            documentation = '?linearRegression'),
    randomForest = list(description = 'Random forest classification, regression or unsupervised',
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
              cat(blue('Modelling'),cli::symbol$continue,'\r',sep = '') 
            }
            params <- x@parameters@modelling
            
            res <- params %>%
              names() %>%
              map(~{
                i <- .
                method <- modellingMethods(names(params)[i])
                
                if (nrow(x@preTreated@data) > 0) {
                  d <- x@preTreated
                } else {
                  d <- x@rawData
                }
                
                newPars <- formals(method) %>%
                  as.list()
                newPars[[1]] <- d
                
                do.call(method,newPars)
              })
              
            x@modelling <- res
            x@log$modelling <- date()
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              cat(blue('Modelling '),'\t\t',green(cli::symbol$tick),' ',elapsed,'\n',sep = '')
            }
            return(x)
          }
)