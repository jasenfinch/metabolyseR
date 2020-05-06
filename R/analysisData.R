#' analysisData
#' @description Create an S4 AnalysisData object.
#' @param data sample data
#' @param info saple info
#' @examples 
#' library(metaboData)
#' a <- analysisData(data = abr1$neg,info = abr1$fact)
#' @export

analysisData <- function(data,info){
  if (nrow(data) != nrow(info)) {
    stop('Number of rows in data should match number of rows in info!')
  }
  
  d <- new('AnalysisData')
  d@data <- data %>%
    as_tibble()
  d@info <- info %>%
    as_tibble()
  return(d)
}

#' dat
#' @rdname dat
#' @description Return or sest sample data in an AnalysisData object.
#' @param x S4 object of class AnalysisData 
#' @export

setMethod('dat',signature = 'AnalysisData',
          function(x){
            x@data
          })

#' @rdname dat
#' @param value tibble containing sample data
#' @export

`dat<-` <- function(x,value){
  x@data <- as_tibble(value)
  return(x)
}

#' sinfo
#' @rdname sinfo
#' @description Return sample info from an AnalysisData object.
#' @param x S4 object of class Data 
#' @export

setMethod('sinfo',signature = 'AnalysisData',
          function(x){
            x@info
          })

#' @rdname sinfo
#' @param value tibble containing sample info
#' @export

`sinfo<-` <- function(x,value){
  x@info <- as_tibble(value)
  return(x)
}

#' features
#' @rdname features
#' @description Return a vector of the feature names.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type return features from \code{raw} or \code{preTreated} data
#' @param ... arguments to pass to the appropriate method
#' @export

setMethod('features',signature = 'AnalysisData',
          function(x){
            x %>%
              dat() %>%
              colnames()
          }) 

#' @rdname features
#' @export

setMethod('features',signature = 'Analysis',
          function(x,type = 'raw'){
            ty <- get(type)
            
            x %>%
              ty() %>%
              features()
          })

#' nFeatures
#' @rdname nFeatures
#' @description Return the number of features.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type return the number features from \code{raw} or \code{preTreated} data
#' @param ... arguments to pass to the appropriate method
#' @export

setMethod('nFeatures',signature = 'AnalysisData',
          function(x){
            x %>%
              features() %>%
              length()
          })

#' @rdname nFeatures
#' @export

setMethod('nFeatures',signature = 'Analysis',
          function(x,type = 'raw'){
            ty <- get(type)
            
            x %>%
              ty() %>%
              nFeatures()
          })

#' nSamples
#' @rdname nSamples
#' @description Return the number of samoles.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type return the number samples from \code{raw} or \code{preTreated} data
#' @param ... arguments to pass to the appropriate method
#' @export

setMethod('nSamples',signature = 'AnalysisData',
          function(x){
            x %>%
              dat() %>%
              nrow()
          })

#' @rdname nSamples
#' @export

setMethod('nSamples',signature = 'Analysis',
          function(x,type = 'raw'){
            ty <- get(type)
            
            x %>%
              ty() %>%
              nSamples()
          })
