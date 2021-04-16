
#' @rdname plotPCA
setGeneric('plotPCA', 
           function(
             analysis, 
             cls = 'class', 
             label = NULL, 
             scale = TRUE, 
             center = TRUE, 
             xAxis = 'PC1', 
             yAxis = 'PC2', 
             shape = FALSE, 
             ellipses = TRUE, 
             title = 'PCA',
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
           {
             standardGeneric('plotPCA')
           })

#' @rdname plotLDA
setGeneric('plotLDA', 
           function(
             analysis, 
             cls = 'class', 
             label = NULL, 
             scale = TRUE, 
             center = TRUE, 
             xAxis = 'DF1', 
             yAxis = 'DF2', 
             shape = FALSE, 
             ellipses = TRUE, 
             title = 'PC-LDA', 
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
           {
             standardGeneric('plotLDA')
           })

#' @rdname plotExplanatoryHeatmap
setGeneric('plotExplanatoryHeatmap',function(x, ...){
  standardGeneric('plotExplanatoryHeatmap')
})

#' @rdname plotSupervisedRF
setGeneric('plotSupervisedRF',
           function(
             x, 
             cls = 'class', 
             rf = list(), 
             label = NULL,
             shape = FALSE, 
             ellipses = TRUE, 
             ROC = TRUE, 
             seed = 1234, 
             title = '', 
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
           {
             standardGeneric('plotSupervisedRF')
           })

#' @rdname plotUnsupervisedRF
setGeneric('plotUnsupervisedRF',
           function(
             x,
             cls = 'class', 
             rf = list(), 
             label = NULL, 
             shape = FALSE, 
             ellipses = TRUE, 
             seed = 1234, 
             title = '', 
             legendPosition = 'bottom', 
             labelSize = 2,
             ...)
           {
             standardGeneric('plotUnsupervisedRF')
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
