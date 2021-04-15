
setGeneric("pre-treatment", function(x) {
  standardGeneric("pre-treatment")
})

setGeneric("modelling", function(x) {
  standardGeneric("modelling")
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

#' @rdname plotOccupancy
setGeneric('plotOccupancy',function(x,cls = 'class', ...){
  standardGeneric('plotOccupancy')
})

#' @rdname anova
setGeneric("anova", 
           function(
             x,
             cls = 'class', 
             pAdjust = 'bonferroni', 
             comparisons = list(), 
             returnModels = FALSE)
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
             returnModels = FALSE) 
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
             seed = 1234)
           {
             standardGeneric("randomForest")
           })


#' @rdname type
setGeneric("type", function(x) {
  standardGeneric("type")
})

#' @rdname response
setGeneric("response", function(x) {
  standardGeneric("response")
})

#' @rdname metrics
setGeneric("metrics", function(x) {
  standardGeneric("metrics")
})

#' @rdname importanceMetrics
setGeneric("importanceMetrics", function(x) {
  standardGeneric("importanceMetrics")
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
