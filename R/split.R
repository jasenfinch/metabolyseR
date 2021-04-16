#' Split an `AnalysisData` object
#' @rdname split
#' @description Split an object of class `AnalysisData` into a list based 
#' a class grouping variable.
#' @param x S4 object of class `AnalysisData`
#' @param cls sample information column to use for splitting
#' @return A list of `AnalysisData` objects.
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact)
#' 
#' ## Split the data set based on the 'day' class information column
#' d <- split(d,cls = 'day')
#' 
#' print(d)
#' @export

setGeneric('split',function(x,cls = 'class'){
  standardGeneric('split')
})

#' @rdname split

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

