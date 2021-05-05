#' `AnalysisData` and `Analysis` class accessors
#' @rdname analysis-accessors
#' @description Accessor methods for the `AnalysisData` and `Analysis` S4 classes.
#' @param x S4 object of class `AnalysisData` or `Analysis`
#' @param type get or set `raw` or `pre-treated` data
#' @param value value to set
#' @param element analysis element results to return
#' @param ... arguments to pass to the appropriate method
#' @section Methods:
#' * `dat`: Return a metabolomic data table.
#' * `dat<-`: Set a metabolomic data table.
#' * `sinfo`: Return a sample information data table.
#' * `sinfo<-`: Set a sample information data table.
#' * `raw`: Return the `AnalysisData` object containing unprocessed metabolomic data from an `Analysis` object.
#' * `raw<-`: Set an `AnalysisData` object to the `raw` slot of an `Analysis` class object.
#' * `preTreated`: Return the `AnalysisData` object containing pre-treated metabolomic data from an `Analysis` object.
#' * `preTreated<-`: Set an `AnalysisData` object to the `pre-treated` slot of an `Analysis` class object.
#' * `features`: Return the features names.
#' * `nSamples`: Return the number of samples.
#' * `nFeatures`: Return the number of features.
#' * `analysisResults`: Return results from an `Analysis` object of an analysis element.
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' ## Return the metabolomic data
#' dat(d)
#' 
#' ## Set the metabolomic data
#' dat(d) <- abr1$neg[,300:400]
#' 
#' ## Return the sample information
#' sinfo(d)
#' 
#' ## Set the sample information
#' sinfo(d) <- abr1$fact
#' 
#' ## Return the feature names
#' features(d)
#' 
#' ## Return the number of samples
#' nSamples(d)
#' 
#' ## Return the number of features
#' nFeatures(d)
#' @export

setGeneric("dat", function(x,...) 
  standardGeneric("dat")
)

#' @rdname analysis-accessors

setMethod('dat',signature = 'AnalysisData',
          function(x){
            x@data
          })

#' @rdname analysis-accessors

setMethod('dat',signature = 'Analysis',
          function(x, type = c('raw','pre-treated')){
            
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated') {
              x %>%
                preTreated() %>%
                dat()
            } else {
              x %>%
                raw() %>%
                dat()
            }
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("dat<-", function(x,...,value) 
  standardGeneric("dat<-")
)

#' @rdname analysis-accessors

setMethod("dat<-",signature = 'AnalysisData',
          function(x,value){
            x@data <- as_tibble(value)
            return(x)
          })

#' @rdname analysis-accessors

setMethod('dat<-',signature = 'Analysis',
          function(x, type = c('raw','pre-treated'), value){
            
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated'){
              d <- preTreated(x)
              dat(d) <- value
              preTreated(x) <- d
            } else {
              d <- raw(x)
              dat(d) <- value
              raw(x) <- d
            }
            
            return(x)
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("sinfo", function(x,...) 
  standardGeneric("sinfo")
)

#' @rdname analysis-accessors

setMethod('sinfo',signature = 'AnalysisData',
          function(x){
            x@info
          }
)

#' @rdname analysis-accessors

setMethod('sinfo',signature = 'Analysis',
          function(x, type = c('raw','pre-treated'), value){
            
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated') {
              x %>%
                preTreated() %>%
                sinfo()
            } else {
              x %>%
                raw() %>%
                sinfo()
            }
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("sinfo<-", function(x,...,value) 
  standardGeneric("sinfo<-")
)

#' @rdname analysis-accessors

setMethod('sinfo<-',signature = 'AnalysisData',
          function(x,value){
            x@info <- as_tibble(value)
            return(x)
          }
)

#' @rdname analysis-accessors

setMethod('sinfo<-',signature = 'Analysis',
          function(x,type = c('raw','pre-treated'), value){
            
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated'){
              d <- preTreated(x)
              sinfo(d) <- value
              preTreated(x) <- d
            } else {
              d <- raw(x)
              sinfo(d) <- value
              raw(x) <- d
            }
            
            return(x)
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("raw", function(x) 
  standardGeneric("raw")
)

#' @rdname analysis-accessors

setMethod('raw',signature = 'Analysis',
          function(x){
            x@raw
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("raw<-", function(x,value)
  standardGeneric("raw<-"))

#' @rdname analysis-accessors

setMethod('raw<-',signature = 'Analysis',
          function(x,value){
            x@raw <- value
            return(x)
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("preTreated", function(x)
  standardGeneric("preTreated"))

#' @rdname analysis-accessors

setMethod('preTreated',signature = 'Analysis',
          function(x){
            x@`pre-treated`
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("preTreated<-", function(x,value)
  standardGeneric("preTreated<-"))

#' @rdname analysis-accessors

setMethod('preTreated<-',signature = 'Analysis',
          function(x,value){
            x@`pre-treated` <- value
            return(x)
          }
)

#' @rdname analysis-accessors
#' @export

setGeneric("features", function(x, ...)
  standardGeneric("features"))

#' @rdname analysis-accessors

setMethod('features',signature = 'AnalysisData',
          function(x){
            x %>%
              dat() %>%
              colnames()
          }) 

#' @rdname analysis-accessors

setMethod('features',signature = 'Analysis',
          function(x,type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated') {
              x %>%
                preTreated() %>%
                features()
            } else {
              x %>%
                raw() %>%
                features()
            }
          })

#' @rdname analysis-accessors
#' @export

setGeneric("nSamples", function(x, ...)
  standardGeneric("nSamples"))

#' @rdname analysis-accessors

setMethod('nSamples',signature = 'AnalysisData',
          function(x){
            x %>%
              dat() %>%
              nrow()
          })

#' @rdname analysis-accessors

setMethod('nSamples',signature = 'Analysis',
          function(x,type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated') {
              x %>%
                preTreated() %>%
                nSamples()
            } else {
              x %>%
                raw() %>%
                nSamples()
            }
          })

#' @rdname analysis-accessors
#' @export

setGeneric("nFeatures", function(x, ...)
  standardGeneric("nFeatures"))

#' @rdname analysis-accessors

setMethod('nFeatures',signature = 'AnalysisData',
          function(x){
            x %>%
              features() %>%
              length()
          }
)

#' @rdname analysis-accessors

setMethod('nFeatures',signature = 'Analysis',
          function(x,type = c('raw','pre-treated')){
            
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'pre-treated') {
              x %>%
                preTreated() %>%
                nFeatures()
            } else {
              x %>%
                raw() %>%
                nFeatures()
            }
            
          })

#' @rdname analysis-accessors
#' @export

setGeneric('analysisResults',function(x,element)
  standardGeneric('analysisResults'))

#' @rdname analysis-accessors

setMethod('analysisResults',signature = 'Analysis',
          function(x,element){
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(str_c('Argument "element" should be one of ',
                         str_c(elements,collapse = ', '),'.'),
                   call. = FALSE)
            }
            
            if (element == 'pre-treatment') {
              d <- preTreated(x)
            } else {
              d <- slot(x,element)
            }
            
            return(d)
          }
)
