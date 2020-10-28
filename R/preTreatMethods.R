
getPreTreatMethods <- function(element = NULL){
  
  elements <- list(
    aggregate = aggregateMethods,
    correction = correctionMethods,
    impute = imputeMethods,
    keep = keepMethods,
    occupancyFilter = occupancyMethods,
    QC = QCMethods,
    remove = removeMethods,
    transform = transformMethods
  )
  
  if (is.null(element)) {
    elements %>%
      return()
  } else {
    if (!(element %in% names(elements))) {
      stop(str_c("Pre-treatment element '",
                 element,
                 "' not recognised. Available elements include: ",
                 str_c(str_c("'",names(elements),"'"),collapse = ' '),'.'))
    }
    
    element <- elements[[element]]
    return(element)
  }
}

#' preTreatmentElements
#' @description Return names of available pre-treatment elements
#' @export

preTreatmentElements <- function(){
  getPreTreatMethods() %>%
    names()
}

#' preTreatmentMethods
#' @description Return names of available methods for a given pre-treatment 
#' element.
#' @param element pre-treatment element
#' @export

preTreatmentMethods <- function(element){
  getPreTreatMethods(element)() %>%
    names() %>%
    sort()
}