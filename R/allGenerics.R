
setGeneric("pre-treatment", function(x) {
  standardGeneric("pre-treatment")
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

#' @rdname raw
setGeneric("raw<-", function(x,value) {
  standardGeneric("raw<-")
})

#' @rdname preTreated
setGeneric("preTreated", function(x) {
  standardGeneric("preTreated")
})

#' @rdname preTreated
setGeneric("preTreated<-", function(x,value) {
  standardGeneric("preTreated<-")
})

#' @rdname plotTIC
setGeneric('plotTIC', 
           function(analysis, 
                    by = 'injOrder', 
                    colour = 'block', 
                    ...){
             standardGeneric('plotTIC')
           })

#' @rdname plotRSD
setGeneric('plotRSD', 
           function(analysis, 
                    cls = 'class', 
                    ...){
             standardGeneric('plotRSD')
           })

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
             labelSize = 2)
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
             labelSize = 2)
           {
             standardGeneric('plotLDA')
           })

#' @rdname plotFeature
setGeneric('plotFeature',
           function(
             analysis, 
             feature,
             cls = 'class', 
             label = NULL, 
             labelSize = 2, 
             ...)
           {
             standardGeneric('plotFeature')
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
             labelSize = 2)
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
             labelSize = 2)
           {
             standardGeneric('plotUnsupervisedRF')
           })

#' @rdname plotOccupancy
setGeneric('plotOccupancy',function(x,cls = 'class', ...){
  standardGeneric('plotOccupancy')
})

#' @rdname dat
setGeneric("dat", function(x,...) {
  standardGeneric("dat")
})

#' @rdname dat
setGeneric("dat<-", function(x,...,value) {
  standardGeneric("dat<-")
})

#' @rdname sinfo
setGeneric("sinfo", function(x,...) {
  standardGeneric("sinfo")
})

#' @rdname sinfo
setGeneric("sinfo<-", function(x,...,value) {
  standardGeneric("sinfo<-")
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
setGeneric("QCoccupancy", 
           function(
             d,
             cls = 'class', 
             QCidx = 'QC', 
             occupancy = 2/3) 
           {
             standardGeneric("QCoccupancy")
           })

#' @rdname QCimpute
setGeneric("QCimpute", 
           function(
             d, 
             cls = 'class', 
             QCidx = 'QC', 
             occupancy = 2/3, 
             parallel = 'variables', 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType(), 
             seed = 1234) 
           {
             standardGeneric("QCimpute")
           })

#' @rdname QCrsdFilter
setGeneric("QCrsdFilter", 
           function(
             d,
             cls = 'class',
             QCidx = 'QC', 
             RSDthresh = 0.5) 
           {
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

#' @rdname removeFeatures
setGeneric("removeFeatures", function(d,features = character()) {
  standardGeneric("removeFeatures")
})

#' @rdname keepSamples
setGeneric("keepSamples", function(d,idx = 'fileOrder', samples = c()) {
  standardGeneric("keepSamples")
})

#' @rdname keepClasses
setGeneric("keepClasses", function(d,cls = 'class', classes = c()) {
  standardGeneric("keepClasses")
})

#' @rdname keepFeatures
setGeneric("keepFeatures", function(d,features = character()) {
  standardGeneric("keepFeatures")
})

#' @rdname  imputeAll
setGeneric("imputeAll", 
           function(
             d, 
             occupancy = 2/3, 
             parallel = 'variables', 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType(), 
             seed = 1234) 
           {
             standardGeneric("imputeAll")
           })

#' @rdname  imputeClass
setGeneric("imputeClass", 
           function(
             d, 
             cls = 'class', 
             occupancy = 2/3, 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType(), 
             seed = 1234) 
           {
             standardGeneric("imputeClass")
           })

#' @rdname  correctionCenter
setGeneric("correctionCenter", 
           function(
             d, 
             block = 'block', 
             type = 'median', 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType()) 
           {
             standardGeneric("correctionCenter")
           })

#' @rdname anova
setGeneric("anova", 
           function(
             x,
             cls = 'class', 
             pAdjust = 'bonferroni', 
             comparisons = list(), 
             returnModels = FALSE, 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType()) 
           {
             standardGeneric("anova")
           })

#' @rdname ttest
setGeneric("ttest", 
           function(
             x,
             cls = 'class', 
             pAdjust = 'bonferroni', 
             comparisons = list(), 
             returnModels = FALSE, 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType()) 
           {
             standardGeneric("ttest")
           })

#' @rdname linearRegression
setGeneric("linearRegression", 
           function(
             x, 
             cls = 'class', 
             pAdjust = 'bonferroni', 
             returnModels = FALSE) 
           {
             standardGeneric("linearRegression")
           })

#' @rdname randomForest
setGeneric("randomForest", 
           function(
             x, 
             cls = 'class',
             rf = list(), 
             reps = 1, 
             binary = FALSE, 
             comparisons = list(), 
             perm = 0, 
             returnModels = FALSE, 
             seed = 1234, 
             nCores = detectCores() * 0.75, 
             clusterType = getClusterType()) 
           {
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
setGeneric("plotMeasures", function(x, response = 'class') {
  standardGeneric("plotMeasures")
})

#' @rdname plotMDS
setGeneric("plotMDS", function(x,cls = 'class', label = NULL, shape = FALSE, ellipses = TRUE, title = '', legendPosition = 'bottom', labelSize = 2) {
  standardGeneric("plotMDS")
})

#' @rdname plotROC
setGeneric("plotROC", function(x, title = '', legendPosition = 'bottom') {
  standardGeneric("plotROC")
})

#' @rdname parameters
setGeneric('parameters',function(x,...){
  standardGeneric('parameters')
})

#' @rdname parameters
setGeneric('parameters<-',function(x,element,value){
  standardGeneric('parameters<-')
})

#' @rdname changeParameter
setGeneric("changeParameter<-", 
           function(
             x,
             parameterName,
             elements = analysisElements(), 
             value) 
           {
             standardGeneric("changeParameter<-")
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

#' @rdname clsArrange
setGeneric("clsArrange", function(x,cls = 'class', descending = FALSE, ...) {
  standardGeneric("clsArrange")
})

#' @rdname clsRename
setGeneric("clsRename", function(x,cls,newName, ...) {
  standardGeneric("clsRename")
})

#' @rdname rsd
setGeneric("rsd", function(x,cls = 'class') {
  standardGeneric("rsd")
})

#' @rdname bindAnalysesRows
setGeneric('bindAnalysesRows',function(x){
  standardGeneric('bindAnalysesRows')
})

#' @rdname split
setGeneric('split',function(x,cls = 'class'){
  standardGeneric('split')
})

#' @rdname binaryComparisons
setGeneric('binaryComparisons',function(x,cls = 'class'){
  standardGeneric('binaryComparisons')
})

#' @rdname exportParameters
setGeneric('exportParameters',function(x,file = 'analysis_parameters.yaml'){
  standardGeneric('exportParameters')
})

#' @rdname analysisResults
setGeneric('analysisResults',function(x,element){
  standardGeneric('analysisResults')
})
