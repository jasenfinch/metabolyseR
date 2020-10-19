#' analysisResults
#' @rdname analysisResults
#' @description Extract analysis results for a given analysis element.
#' @param x S4 object of class Analysis
#' @param element Analysis element to extract. 
#' Should be one of those returned \code{analysisElements()}.
#' @export

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

#' preTreated
#' @rdname preTreated
#' @description Get or set an AnalysisData object from 
#' the pre-treated slot of the Analysis class.
#' @param x S4 object of class Analysis
#' @param value S4 object of class AnalysisData 
#' @export 

setMethod('preTreated',signature = 'Analysis',
          function(x){
            x@`pre-treated`
          }
)

#' @rdname preTreated
#' @export

setMethod('preTreated<-',signature = 'Analysis',
          function(x,value){
            x@`pre-treated` <- value
            return(x)
          }
)

#' raw
#' @rdname raw
#' @description Get or set an AnalysisData object from 
#' the raw slot of the Analysis class.
#' @param x S4 object of class Analysis
#' @param value S4 object of class AnalysisData 
#' @export 

setMethod('raw',signature = 'Analysis',
          function(x){
            x@raw
          }
)

#' @rdname raw
#' @export

setMethod('raw<-',signature = 'Analysis',
          function(x,value){
            x@raw <- value
            return(x)
          }
)


#' dat
#' @rdname dat
#' @description Return or set sample data in an 
#' AnalysisData or Analysis objects.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type data type to extract or set. 
#' Should be one of "raw" or "pre-treated" 
#' @param ... arguments to pass to the appropriate method
#' @param value tibble containing sample data
#' @export

setMethod('dat',signature = 'AnalysisData',
          function(x){
            x@data
          })

#' @rdname dat
#' @export

setMethod("dat<-",signature = 'AnalysisData',
          function(x,value){
            x@data <- as_tibble(value)
            return(x)
          })

#' @rdname dat
#' @export

setMethod('dat',signature = 'Analysis',
          function(x, type = 'pre-treated'){
            
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be one of "raw" or "pre-treated".',
                   call. = FALSE)
            }
            
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

#' @rdname dat
#' @export

setMethod('dat<-',signature = 'Analysis',
          function(x, type = 'pre-treated', value){
            
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be one of "raw" or "pre-treated".',
                   call. = FALSE)
            }
            
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

#' sinfo
#' @rdname sinfo
#' @description Return sample info from an AnalysisData or Analysis object.
#' @param x S4 object of class AnalysisData or Analysis
#' @param type sample information type to extract or set. 
#' Should be one of "raw" or "pre-treated" 
#' @param ...  arguments to pass to the appropriate method
#' @param value tibble containing sample info
#' @export

setMethod('sinfo',signature = 'AnalysisData',
          function(x){
            x@info
          })

#' @rdname sinfo
#' @export

setMethod('sinfo<-',signature = 'AnalysisData',
          function(x,value){
            x@info <- as_tibble(value)
            return(x)
          }
)

#' @rdname sinfo
#' @export

setMethod('sinfo',signature = 'Analysis',
          function(x, type = 'raw', value){
            
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be one of "raw" or "pre-treated".',
                   call. = FALSE)
            }
            
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

#' @rdname sinfo
#' @export

setMethod('sinfo<-',signature = 'Analysis',
          function(x,type = 'raw', value){
            
            if (!(type %in% c('raw','pre-treated'))) {
              stop('Argument "type" should be one of "raw" or "pre-treated".',
                   call. = FALSE)
            }
            
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
