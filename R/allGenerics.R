
setGeneric("preTreat", function(x) {
  standardGeneric("preTreat")
})


setGeneric("classification", function(x) {
  standardGeneric("classification")
})


setGeneric("featureSelection", function(x) {
  standardGeneric("featureSelection")
})


setGeneric("correlations", function(x) {
  standardGeneric("correlations")
})

#' @rdname rawData
setGeneric("rawData", function(x) {
  standardGeneric("rawData")
})

#' @rdname preTreatedData
setGeneric("preTreatedData", function(x) {
  standardGeneric("preTreatedData")
})

#' @rdname classificationResults
setGeneric("classificationResults", function(x) {
  standardGeneric("classificationResults")
})

#' @rdname featureSelectionResults
setGeneric("featureSelectionResults", function(x) {
  standardGeneric("featureSelectionResults")
})

#' @rdname correlationResults
setGeneric("correlationResults", function(x) {
  standardGeneric("correlationResults")
})

#' @rdname plotFeatureSelection
setGeneric('plotFeatureSelection',function(analysis, method = 'fs.rf', mz = T, modes = T, interactive = F){
  standardGeneric('plotFeatureSelection')
})