
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

#' @rdname rawInfo
setGeneric("rawInfo", function(x) {
  standardGeneric("rawInfo")
})

#' @rdname preTreatedData
setGeneric("preTreatedData", function(x) {
  standardGeneric("preTreatedData")
})

#' @rdname preTreatedInfo
setGeneric("preTreatedInfo", function(x) {
  standardGeneric("preTreatedInfo")
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

#' @rdname plotTIC
setGeneric('plotTIC', function(analysis, by = 'injOrder', colour = 'block', modes = T){
  standardGeneric('plotTIC')
})

#' @rdname plotRSD
setGeneric('plotRSD', function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, modes = T, histBins = 30){
  standardGeneric('plotRSD')
})

#' @rdname plotPCA
setGeneric('plotPCA', function(analysis, cls = 'class', scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2'){
  standardGeneric('plotPCA')
})

#' @rdname plotLDA
setGeneric('plotLDA', function(analysis, cls = 'class', scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2'){
  standardGeneric('plotLDA')
})

#' @rdname plotClassification
setGeneric('plotClassification',function(analysis,method = 'randomForest'){
  standardGeneric('plotClassification')
})

#' @rdname plotClassificationDendrogram
setGeneric('plotClassificationDendrogram',function(analysis,method = 'randomForest', measure = 'Margin', clusterMethod = 'ward.D2'){
  standardGeneric('plotClassificationDendrogram')
})

#' @rdname plotFeatureSelection
setGeneric('plotFeatureSelection',function(analysis, method = 'fs.rf', mz = T, modes = T, pairwises = NULL){
  standardGeneric('plotFeatureSelection')
})

#' @rdname plotFeature
setGeneric('plotFeature',function(analysis, feature, cls = 'class'){
  standardGeneric('plotFeature')
})

#' @rdname plotExplanatoryHeatmap
setGeneric('plotExplanatoryHeatmap',function(analysis, method = 'fs.rf', threshold = 0.01, pairwises = NULL, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', colour = ptol_pal()(1)){
  standardGeneric('plotExplanatoryHeatmap')
})

#' @rdname plotSupervisedRF
setGeneric('plotSupervisedRF',function(analysis, cls = 'class', label = NULL, ellipses = T, seed = 1234, title = 'MDS plot of a supervised random forest', legendPosition = 'bottom', labelSize = 2, ...){
  standardGeneric('plotSupervisedRF')
})

#' @rdname plotUnsupervisedRF
setGeneric('plotUnsupervisedRF',function(analysis,cls = 'class', ...){
  standardGeneric('plotUnsupervisedRF')
})