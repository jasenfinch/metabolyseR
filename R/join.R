#' Bind `AnalysisData` objects by row
#' @rdname bind
#' @description Bind the rows of AnalysisData objects contained within a list.
#' @param d list object containing S4 objects of class AnalysisData to be bound
#' @return An S4 object of class AnalysisData containg the bound data sets.
#' @examples 
#' library(metaboData)
#' d <- list(
#'  negative = analysisData(abr1$neg,abr1$fact),
#'  positive = analysisData(abr1$pos,abr1$fact)
#'  )
#'
#' bindRows(d)
#' @export

setGeneric('bindRows',function(d){
  standardGeneric('bindRows')
})

#' @rdname bind

setMethod('bindRows',signature = 'list',function(d){
  object_classes <- d %>%
    map_chr(class)
  
  if (FALSE %in% (object_classes == 'AnalysisData')) {
    stop(
      str_c('All objects contained within supplied list should ',
            'be of class AnalysisData'),
      call. = FALSE)
  }
  
  sample_info <- d %>%
    map(sinfo) %>%
    bind_rows()
  
  sample_data <- d %>%
    map(dat) %>%
    bind_rows()
  
  d <- analysisData(sample_data,sample_info)
  
  return(d)
})

