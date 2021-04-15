#' @importFrom missForest missForest
#' @importFrom stringr str_replace_all

setMethod("pre-treatment", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == TRUE) {
              startTime <- proc.time()
              message(blue('Pre-treatment '),
                      cli::symbol$continue,'\r',
                      appendLF = FALSE) 
            }
            params <- x %>%
              parameters() %>%
              parameters('pre-treatment')
            d <- raw(x)
            
            for (i in seq_along(params)) {
              method <- getPreTreatMethods(names(params)[i])
              m <- lapply(names(params[[i]]),method)
              for (j in seq_along(m)) {
                newPars <- formals(m[[j]])
                if (!(length(params[[i]][[j]]) == 0)) {
                  newPars[names(params[[i]][[j]])] <- params[[i]][[j]]
                }
                newPars[[1]] <- d
                d <- do.call(m[[j]],newPars %>% as.list())
              }
            }
            preTreated(x) <- d
            x@log$preTreatment <- date()
            
            if (verbose == TRUE) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              message('\r',
                      blue('Pre-treatment '),
                      '\t',
                      green(cli::symbol$tick),
                      ' ',
                      elapsed)
            }
            return(x)
          }
)

aggregateMethods <- function(method = NULL){
  
  methods <- list(
    sum = aggregateSum,
    mean = aggregateMean,
    median = aggregateMedian
  )
  
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
  return(method)
}

keepMethods <- function(method = NULL){
  methods <- list(
    samples = keepSamples,
    classes = keepClasses,
    features = keepFeatures
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    if (!(method %in% names(methods))) {
      stop(str_c("Keep method '",
                 method,
                 "' not recognised. Available methods include: ",
                 str_c(str_c("'",names(methods),"'"),collapse = ' '),'.'))
    }
    method <- methods[[method]]
  }
  
  return(method)
}

removeMethods <- function(method = NULL, description = FALSE){
  methods <- list(
    samples = removeSamples,
    classes = removeClasses,
    features = removeFeatures 
  )
  
  if (is.null(method)) {
    method <- methods
  } else {
    if (!(method %in% names(methods))) {
      stop(str_c("Remove method '",
                 method,
                 "' not recognised. Available methods include: ",
                 str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
    }
    method <- methods[[method]]
  }
  
  return(method)
}

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