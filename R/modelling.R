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

setGeneric("modelling", function(x)
  standardGeneric("modelling"))

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
    filter(metric == current_metric)
  
  if (metric == 'false_positive_rate') {
    explan <- explan %>%
      filter(value < threshold) %>% 
      arrange(value)
  } else {
    explan <- explan %>%
      filter(value > threshold) %>% 
      arrange(desc(value))
  }
  
  return(explan)
}

explanatoryFeaturesRegression <- function(x,metric,threshold){
  
  current_metric <- metric
  
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
    filter(metric == current_metric) %>%
    filter(value > threshold) %>% 
    arrange(desc(value))
  
  return(explan)
}
