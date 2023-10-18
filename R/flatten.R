#' Flatten an `AnalysisData` object
#' @rdname flatten
#' @description
#' Flatten an `AnalysisData` S4 class object into a single tibble.
#' @param x An object of S4 class `AnalysisData`
#' @examples
#' d <- analysisData(
#'   metaboData::abr1$neg,
#'   metaboData::abr1$fact
#' )
#' 
#' flatten(d)
#' @export

setGeneric('flatten',function(x) standardGeneric('flatten'))

#' @rdname flatten

setMethod('flatten',signature = 'AnalysisData',function(x){
  bind_cols(
    sinfo(x),
    dat(x)
  )
})
