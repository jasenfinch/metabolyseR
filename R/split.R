#' split
#' @rdname split
#' @description Split an object of class AnalysisData into a list based 
#' given class information.
#' @param x S4 object of class AnalysisData
#' @param cls sample information column to use for splitting
#' @examples 
#' library(metaboData)
#' d <- analysisData(abr1$neg,abr1$fact)
#' d <- split(d,cls = 'day')
#' @export

setMethod('split',signature = 'AnalysisData',function(x,cls = 'class'){
  ind_classes <- x %>%
    clsExtract(cls = cls) %>%
    unique() %>%
    sort()
  
  d <- ind_classes %>%
    map(keepClasses,d = x,cls = cls) %>%
    set_names(ind_classes)
  
  return(d)
})

