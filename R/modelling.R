
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