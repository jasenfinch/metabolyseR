
#' @rdname plotExplanatoryHeatmap
setGeneric('plotExplanatoryHeatmap',function(x, ...){
  standardGeneric('plotExplanatoryHeatmap')
})

#' @rdname plotImportance
setGeneric("plotImportance", function(x,...) {
  standardGeneric("plotImportance")
})

#' @rdname plotMetrics
setGeneric("plotMetrics", function(x, response = 'class') {
  standardGeneric("plotMetrics")
})

#' @rdname plotMDS
setGeneric("plotMDS", 
           function(
             x,
             cls = 'class', 
             label = NULL, 
             shape = FALSE, 
             ellipses = TRUE, 
             title = '', 
             legendPosition = 'bottom', 
             labelSize = 2) 
           {
             standardGeneric("plotMDS")
           })

#' @rdname plotROC
setGeneric("plotROC", function(x, title = '', legendPosition = 'bottom') {
  standardGeneric("plotROC")
})
