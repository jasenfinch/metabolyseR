
setGeneric("preTreat", function(x) {
  standardGeneric("preTreat")
})

setGeneric("modelling", function(x) {
  standardGeneric("modelling")
})

#' @rdname correlations
setGeneric("correlations", function(x,...) {
  standardGeneric("correlations")
})

#' @rdname raw
setGeneric("raw", function(x) {
  standardGeneric("raw")
})

#' @rdname rawData
setGeneric("rawData", function(x) {
  standardGeneric("rawData")
})

#' @rdname rawInfo
setGeneric("rawInfo", function(x) {
  standardGeneric("rawInfo")
})

#' @rdname preTreated
setGeneric("preTreated", function(x) {
  standardGeneric("preTreated")
})

#' @rdname preTreatedData
setGeneric("preTreatedData", function(x) {
  standardGeneric("preTreatedData")
})

#' @rdname preTreatedInfo
setGeneric("preTreatedInfo", function(x) {
  standardGeneric("preTreatedInfo")
})

#' @rdname modellingResults
setGeneric("modellingResults", function(x) {
  standardGeneric("modellingResults")
})

#' @rdname correlationResults
setGeneric("correlationResults", function(x) {
  standardGeneric("correlationResults")
})

#' @rdname plotTIC
setGeneric('plotTIC', function(analysis, by = 'injOrder', colour = 'block', ...){
  standardGeneric('plotTIC')
})

#' @rdname plotRSD
setGeneric('plotRSD', function(analysis, cls = 'class', ...){
  standardGeneric('plotRSD')
})

#' @rdname plotPCA
setGeneric('plotPCA', function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2', ellipses = T, title = 'Principle Component Analysis (PCA) plot', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
  standardGeneric('plotPCA')
})

#' @rdname plotLDA
setGeneric('plotLDA', function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2', ellipses = T, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA) plot', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
  standardGeneric('plotLDA')
})

#' @rdname plotFeature
setGeneric('plotFeature',function(analysis, feature, cls = 'class', label = NULL, labelSize = 2, ...){
  standardGeneric('plotFeature')
})

#' @rdname plotExplanatoryHeatmap
setGeneric('plotExplanatoryHeatmap',function(x, ...){
  standardGeneric('plotExplanatoryHeatmap')
})

#' @rdname plotSupervisedRF
setGeneric('plotSupervisedRF',function(x, cls = 'class', rf = list(), label = NULL, ellipses = T, ROC = T, seed = 1234, title = 'MDS plot of a supervised random forest', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
  standardGeneric('plotSupervisedRF')
})

#' @rdname plotUnsupervisedRF
setGeneric('plotUnsupervisedRF',function(x,cls = 'class', rf = list(), label = NULL, ellipses = T, seed = 1234, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
  standardGeneric('plotUnsupervisedRF')
})

#' @rdname plotOccupancy
setGeneric('plotOccupancy',function(x,cls = 'class', ...){
  standardGeneric('plotOccupancy')
})

#' @rdname dat
setGeneric("dat", function(x) {
  standardGeneric("dat")
})

#' @rdname sinfo
setGeneric("sinfo", function(x) {
  standardGeneric("sinfo")
})

#' @rdname features
setGeneric("features", function(x, ...) {
  standardGeneric("features")
})

#' @rdname nFeatures
setGeneric("nFeatures", function(x, ...) {
  standardGeneric("nFeatures")
})

#' @rdname nSamples
setGeneric("nSamples", function(x, ...) {
  standardGeneric("nSamples")
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

#' @rdname occupancy
setGeneric("occupancy", function(x, cls = 'class') {
  standardGeneric("occupancy")
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
setGeneric("QCimpute", function(d, cls = 'class', QCidx = 'QC', occupancy = 2/3, parallel = 'variables', nCores = detectCores() * 0.75, clusterType = getClusterType(), seed = 1234) {
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

#' @rdname keepSamples
setGeneric("keepSamples", function(d,idx = 'fileOrder', samples = c()) {
  standardGeneric("keepSamples")
})

#' @rdname keepClasses
setGeneric("keepClasses", function(d,cls = 'class', classes = c()) {
  standardGeneric("keepClasses")
})

#' @rdname keepVariables
setGeneric("keepVariables", function(d,variables = character()) {
  standardGeneric("keepVariables")
})

#' @rdname  imputeAll
setGeneric("imputeAll", function(d, occupancy = 2/3, parallel = 'variables', nCores = detectCores() * 0.75, clusterType = getClusterType(), seed = 1234) {
  standardGeneric("imputeAll")
})

#' @rdname  imputeClass
setGeneric("imputeClass", function(d, cls = 'class', occupancy = 2/3, nCores = detectCores() * 0.75, clusterType = getClusterType(), seed = 1234) {
  standardGeneric("imputeClass")
})

#' @rdname  correctionCenter
setGeneric("correctionCenter", function(d, block = 'block', type = 'median', nCores = detectCores() * 0.75, clusterType = getClusterType()) {
  standardGeneric("correctionCenter")
})

#' @rdname anova
setGeneric("anova", function(x,cls = 'class', pAdjust = 'bonferroni', comparisons = list(), returnModels = F, nCores = detectCores() * 0.75, clusterType = getClusterType()) {
  standardGeneric("anova")
})

#' @rdname ttest
setGeneric("ttest", function(x,cls = 'class', pAdjust = 'bonferroni', comparisons = list(), returnModels = F, nCores = detectCores() * 0.75, clusterType = getClusterType()) {
  standardGeneric("ttest")
})

#' @rdname linearRegression
setGeneric("linearRegression", function(x, cls = 'class', pAdjust = 'bonferroni', returnModels = F) {
  standardGeneric("linearRegression")
})

#' @rdname randomForest
setGeneric("randomForest", function(x, cls = 'class', rf = list(), reps = 1, binary = F, comparisons = list(), perm = 0, returnModels = F, seed = 1234, nCores = detectCores() * 0.75, clusterType = getClusterType()) {
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

#' @rdname explanatoryFeatures
setGeneric('explanatoryFeatures', function(x,...) {
  standardGeneric("explanatoryFeatures")
})

#' @rdname plotImportance
setGeneric("plotImportance", function(x,...) {
  standardGeneric("plotImportance")
})

#' @rdname plotMeasures
setGeneric("plotMeasures", function(x, predictor = 'class') {
  standardGeneric("plotMeasures")
})

#' @rdname plotMDS
setGeneric("plotMDS", function(x,cls = 'class', label = NULL, ellipses = T, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2) {
  standardGeneric("plotMDS")
})

#' @rdname plotROC
setGeneric("plotROC", function(x, title = '', legend = TRUE) {
  standardGeneric("plotROC")
})

#' @rdname changeParameter
setGeneric("changeParameter", function(parameters,parameterName,newValue,elements = c('preTreat','modelling','correlations')) {
  standardGeneric("changeParameter")
})

#' @rdname clsAvailable
setGeneric("clsAvailable", function(x,...) {
  standardGeneric("clsAvailable")
})

#' @rdname clsExtract
setGeneric("clsExtract", function(x,cls = 'class', ...) {
  standardGeneric("clsExtract")
})

#' @rdname clsReplace
setGeneric("clsReplace", function(x,value,cls = 'class', ...) {
  standardGeneric("clsReplace")
})

#' @rdname clsAdd
setGeneric("clsAdd", function(x,cls,value,...) {
  standardGeneric("clsAdd")
})

#' @rdname clsRemove
setGeneric("clsRemove", function(x,cls,...) {
  standardGeneric("clsRemove")
})

#' @rdname rsd
setGeneric("rsd", function(x,cls = 'class') {
  standardGeneric("rsd")
})
