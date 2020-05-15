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
  dat(d) <- data %>%
    as_tibble()
  sinfo(d) <- info %>%
    as_tibble()
  return(d)
}

#' features
#' @rdname features
#' @description Return a vector of the feature names.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type return features from "raw" or "pre-treated" data
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
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be "raw" or "pre-treated".',call. = FALSE)
            }
            
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

#' nFeatures
#' @rdname nFeatures
#' @description Return the number of features.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type return features from "raw" or "pre-treated" data
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
            
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be "raw" or "pre-treated".',call. = FALSE)
            }
            
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

#' nSamples
#' @rdname nSamples
#' @description Return the number of samoles.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type return features from "raw" or "pre-treated" data
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
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be "raw" or "pre-treated".',call. = FALSE)
            }
            
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
