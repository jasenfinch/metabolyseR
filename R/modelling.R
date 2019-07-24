
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