#' bindAnalysesRows
#' @rdname bindAnalysesRows
#' @description Bind rows of objects of class AnalysisData contained within a list.
#' @param x list object containing S4 objects of class AnalysisData to be bound
#' @export

setMethod('bindAnalysesRows',signature = 'list',function(x){
  object_classes <- x %>%
    map_chr(class)
  
  if (F %in% (object_classes == 'AnalysisData')) {
    stop('All objects contained within supplied list should be of class AnalysisData',call. = FALSE)
  }
  
  sample_info <- x %>%
    map(sinfo) %>%
    bind_rows()
  
  sample_data <- x %>%
    map(dat) %>%
    bind_rows()
  
  d <- analysisData(sample_data,sample_info)
  
  return(d)
})
