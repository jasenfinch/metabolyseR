
setGeneric("preTreat", function(x) {
  standardGeneric("preTreat")
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

#' @rdname correlationResults
setGeneric("correlationResults", function(x) {
  standardGeneric("correlationResults")
})

#' @rdname plotTIC
setGeneric('plotTIC', function(analysis, by = 'injOrder', colour = 'block', modes = T){
  standardGeneric('plotTIC')
})

#' @rdname plotRSD
setGeneric('plotRSD', function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, modes = T, histBins = 30, title = 'Relative standard deviation distributions'){
  standardGeneric('plotRSD')
})

#' @rdname plotPCA
setGeneric('plotPCA', function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2', ellipses = T, title = 'Principle Component Analysis (PCA) plot', legendPosition = 'bottom', labelSize = 2){
  standardGeneric('plotPCA')
})

#' @rdname plotLDA
setGeneric('plotLDA', function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2', ellipses = T, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA) plot', legendPosition = 'bottom', labelSize = 2){
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
setGeneric('plotFeature',function(analysis, feature, cls = 'class', label = NULL, labelSize = 2){
  standardGeneric('plotFeature')
})

#' @rdname plotExplanatoryHeatmap
setGeneric('plotExplanatoryHeatmap',function(analysis, method = 'fs.rf', threshold = 0.01, pairwises = NULL, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', low = 'white', high = "#F21A00", featureNames = T){
  standardGeneric('plotExplanatoryHeatmap')
})

#' @rdname plotSupervisedRF
setGeneric('plotSupervisedRF',function(analysis, cls = 'class', label = NULL, ellipses = T, ROC = T, seed = 1234, title = 'MDS plot of a supervised random forest', legendPosition = 'bottom', labelSize = 2, ...){
  standardGeneric('plotSupervisedRF')
})

#' @rdname plotUnsupervisedRF
setGeneric('plotUnsupervisedRF',function(analysis,cls = 'class', label = NULL, ellipses = T, title = 'MDS plot of an unsupervised random forest', legendPosition = 'bottom', labelSize = 2, seed = 1234, ...){
  standardGeneric('plotUnsupervisedRF')
})

#' @rdname dat
setGeneric("dat", function(x) {
  standardGeneric("dat")
})

#' @rdname sinfo
setGeneric("sinfo", function(x) {
  standardGeneric("sinfo")
})

#' @rdname aggregateSum
setGeneric("aggregateSum", function(d,cls = 'class') {
  standardGeneric("aggregateSum")
})

#' @rdname aggregateMean
setGeneric("aggregateMean", function(d,cls = 'class') {
  standardGeneric("aggregateMean")
})

#' @rdname aggregateMedian
setGeneric("aggregateMedian", function(d,cls = 'class') {
  standardGeneric("aggregateMedian")
})

#' @rdname occupancyMaximum
setGeneric("occupancyMaximum", function(dat, cls = 'class', occupancy = 2/3) {
  standardGeneric("occupancyMaximum")
})

#' @rdname occupancyMinimum
setGeneric("occupancyMinimum", function(dat, cls = 'class', occupancy = 2/3) {
  standardGeneric("occupancyMinimum")
})

#' @rdname transformCenter
setGeneric("transformCenter", function(d) {
  standardGeneric("transformCenter")
})

#' @rdname transformAuto
setGeneric("transformAuto", function(d) {
  standardGeneric("transformAuto")
})

#' @rdname transformRange
setGeneric("transformRange", function(d) {
  standardGeneric("transformRange")
})

#' @rdname transformPareto
setGeneric("transformPareto", function(d) {
  standardGeneric("transformPareto")
})

#' @rdname transformLevel
setGeneric("transformLevel", function(d) {
  standardGeneric("transformLevel")
})

#' @rdname transformVast
setGeneric("transformVast", function(d) {
  standardGeneric("transformVast")
})

#' @rdname transformLn
setGeneric("transformLn", function(d,add = 1) {
  standardGeneric("transformLn")
})

#' @rdname transformLog10
setGeneric("transformLog10", function(d,add = 1) {
  standardGeneric("transformLog10")
})
    
#' @rdname transformSQRT
setGeneric("transformSQRT", function(d) {
  standardGeneric("transformSQRT")
})
    
#' @rdname transformArcSine
setGeneric("transformArcSine", function(d) {
  standardGeneric("transformArcSine")
})

#' @rdname transformTICnorm
setGeneric("transformTICnorm", function(d) {
  standardGeneric("transformTICnorm")
})

#' @rdname QCoccupancy
setGeneric("QCoccupancy", function(d,cls = 'class', QCidx = 'QC', occupancy = 2/3) {
  standardGeneric("QCoccupancy")
})

#' @rdname QCimpute
setGeneric("QCimpute", function(d, cls = 'class', QCidx = 'QC', occupancy = 2/3, parallel = 'variables', nCores = detectCores(), clusterType = 'PSOCK', seed = 1234) {
  standardGeneric("QCimpute")
})

#' @rdname QCrsdFilter
setGeneric("QCrsdFilter", function(d,cls = 'class', QCidx = 'QC', RSDthresh = 0.5) {
  standardGeneric("QCrsdFilter")
})

#' @rdname QCremove
setGeneric("QCremove", function(d,cls = 'class', QCidx = 'QC') {
  standardGeneric("QCremove")
})

#' @rdname removeSamples
setGeneric("removeSamples", function(d,idx = 'fileOrder', samples = c()) {
  standardGeneric("removeSamples")
})

#' @rdname removeClasses
setGeneric("removeClasses", function(d,cls = 'class', classes = c()) {
  standardGeneric("removeClasses")
})

#' @rdname removeVariables
setGeneric("removeVariables", function(d,variables = character()) {
  standardGeneric("removeVariables")
})

#' @rdname  imputeAll
setGeneric("imputeAll", function(d, occupancy = 2/3, parallel = 'variables', nCores = detectCores() * 0.75, clusterType = 'FORK', seed = 1234) {
  standardGeneric("imputeAll")
})

#' @rdname  imputeClass
setGeneric("imputeClass", function(d, cls = 'class', occupancy = 2/3, nCores = detectCores() * 0.75, clusterType = 'FORK', seed = 1234) {
  standardGeneric("imputeClass")
})

#' @rdname  correctionCenter
setGeneric("correctionCenter", function(d, block = 'block', type = 'median') {
  standardGeneric("correctionCenter")
})

#' @rdname ttest
setGeneric("ttest", function(x,cls,pAdjust = 'bonferroni', returnModels = F, nCores = detectCores() * 0.75, clusterType = clusterType(), ...) {
  standardGeneric("ttest")
})

#' @rdname linearRegression
setGeneric("linearRegression", function(x, cls, pAdjust = 'bonferroni', returnModels = F) {
  standardGeneric("linearRegression")
})

#' @rdname randomForest
setGeneric("randomForest", function(x, cls = NULL, rf = list(), reps = 1, binary = F, comparisons = list(), perm = 0, returnModels = F, seed = 1234, nCores = detectCores() * 0.75, clusterType = getClusterType()) {
  standardGeneric("randomForest")
})

#' @rdname measures
setGeneric("measures", function(x) {
  standardGeneric("measures")
})

#' @rdname importance
setGeneric("importance", function(x) {
  standardGeneric("importance")
})


