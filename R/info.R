#' Sample meta information wrangling
#' @rdname cls
#' @description Query or alter sample meta information in `AnalysisData` or `Analysis` class objects.
#' @param d S4 object of class Analysis or AnalysisData
#' @param cls sample info column to extract
#' @param value vactor of new sample information for replacement
#' @param type `raw` or `pre-treated` sample information
#' @param descending TRUE/FALSE, arrange samples in descending order
#' @param newName new column name
#' @param ... arguments to pass to specific method
#' @section Methods:
#' * `clsAdd`: Add a sample information column.
#' * `clsArrange`: Arrange sample row order by a specified sample information column.
#' * `clsAvailable`: Retrieve the names of the available sample information columns.
#' * `clsExtract`: Extract the values of a specified sample information column.
#' * `clsRemove`: Remove a sample information column.
#' * `clsRename`: Rename a sample information column.
#' * `clsReplace`: Replace a sample information column.
#' @examples 
#' library(metaboData)
#' d <- analysisData(abr1$neg,abr1$fact)
#' 
#' ## Add a sample information column named 'new'
#' d <- clsAdd(d,'new',1:nSamples(d))
#' 
#' print(d)
#' 
#' ## Arrange the row orders by the 'day' column
#' d <- clsArrange(d,'day')
#' 
#' clsExtract(d,'day')
#' 
#' ## Retreive the available sample information column names
#' clsAvailable(d)
#' 
#' ## Extract the values of the 'day' column
#' clsExtract(d,'day')
#' 
#' ## Remove the 'class' column
#' d <- clsRemove(d,'class')
#' 
#' clsAvailable(d)
#' 
#' ## Rename the 'day' column to 'treatment'
#' d <- clsRename(d,'day','treatment')
#' 
#' clsAvailable(d)
#' 
#' ## Replace the values of the 'treatment' column
#' d <- clsReplace(d,rep(1,nSamples(d)),'treatment')
#' 
#' clsExtract(d,'treatment')
#' @export

setGeneric("clsAdd", function(d,cls,value,...)
  standardGeneric("clsAdd"))

#' @rdname cls
#' @importFrom rlang :=

setMethod('clsAdd',
          signature = 'AnalysisData',
          function(d,cls,value){
            if (cls %in% clsAvailable(d)) {
              stop(
                str_c('Class information column "',
                      cls,
                      '" already present.'),
                call. = FALSE)
            }
            
            sinfo(d) <- d %>%
              sinfo() %>%
              mutate({{cls}} := value)
            return(d)
          })

#' @rdname cls

setMethod('clsAdd',
          signature = 'Analysis',
          function(d,cls,value,type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                               'pre-treated'))
            
            if (type == 'raw'){
              sl <- get('raw')  
              `sl<-` <- get(str_c('raw','<-'))
            } else {
              sl <- get('preTreated')
              `sl<-` <- get(str_c('preTreated','<-'))
            }
            
            sl(d) <- d %>%
              sl() %>%
              clsAdd(value = value,cls = cls)
            
            return(d)
          })

#' @rdname cls
#' @export

setGeneric("clsArrange", function(d,cls = 'class', descending = FALSE, ...)
  standardGeneric("clsArrange"))

#' @rdname cls
#' @importFrom dplyr desc

setMethod('clsArrange',
          signature = 'AnalysisData',
          function(d,cls = 'class', descending = FALSE){
            
            sample_data <- dat(d) %>%
              bind_cols(sinfo(d) %>%
                          select(all_of(cls)))
            
            sample_info <- sinfo(d)
            
            if (isTRUE(descending)) {
              sample_data <- sample_data %>%
                arrange(desc(!!sym(cls)))
              
              sample_info <- sample_info %>%
                arrange(desc(!!sym(cls)))
            } else {
              sample_data <- sample_data %>%
                arrange(!!sym(cls))
              
              sample_info <- sample_info %>%
                arrange(!!sym(cls))
            }
            
            dat(d) <- sample_data %>%
              select(-all_of(cls))
            sinfo(d) <- sample_info
            
            return(d)
          })

#' @rdname cls

setMethod('clsArrange',
          signature = 'Analysis',
          function(d,cls = 'class', descending = FALSE, type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'raw'){
              sl <- get('raw')  
              `sl<-` <- get(str_c('raw','<-'))
            } else {
              sl <- get('preTreated')
              `sl<-` <- get(str_c('preTreated','<-'))
            }
            
            sl(d) <- d %>%
              sl() %>%
              clsArrange(cls = cls,descending = descending)
          })

#' @rdname cls
#' @export

setGeneric("clsAvailable", function(d,...)
  standardGeneric("clsAvailable"))

#' @rdname cls

setMethod('clsAvailable',signature = 'AnalysisData',function(d){
  d %>%
    sinfo() %>%
    colnames()
})

#' @rdname cls

setMethod('clsAvailable',signature = 'Analysis',function(d,type = c('raw','pre-treated')){
  type <- match.arg(type,
                    choices = c('raw',
                                'pre-treated'))
  
  if (type == 'raw'){
    sl <- get('raw')  
  } else {
    sl <- get('preTreated')
  }
  
  d %>%
    sl() %>%
    clsAvailable()
})

#' @rdname cls
#' @export

setGeneric("clsExtract", function(d,cls = 'class', ...)
  standardGeneric("clsExtract"))

#' @rdname cls

setMethod('clsExtract',
          signature = 'AnalysisData',
          function(d, cls = 'class'){
            d %>%
              sinfo() %>%
              select(all_of(cls)) %>%
              deframe()
          })

#' @rdname cls

setMethod('clsExtract',
          signature = 'Analysis',
          function(d,cls = 'class',type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'raw'){
              sl <- get('raw')  
            } else {
              sl <- get('preTreated')
            }
            
            d %>%
              sl() %>%
              clsExtract(cls = cls)
          })

#' @rdname cls
#' @export

setGeneric("clsRemove", function(d,cls,...)
  standardGeneric("clsRemove"))

#' @rdname cls

setMethod('clsRemove',signature = 'AnalysisData',function(d,cls){
  if (!(cls %in% clsAvailable(d))) {
    stop(str_c('Class information column "',cls,'" not present.'),
         call. = FALSE)
  }
  
  sinfo(d) <- d %>%
    sinfo() %>%
    select(-all_of(cls))
  
  return(d)
})

#' @rdname cls

setMethod('clsRemove',
          signature = 'Analysis',
          function(d,cls,type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'raw'){
              sl <- get('raw')  
              `sl<-` <- get(str_c('raw','<-'))
            } else {
              sl <- get('preTreated')
              `sl<-` <- get(str_c('preTreated','<-'))
            }
            
            sl(d) <- d %>%
              sl() %>%
              clsRemove(cls)
            
            return(d)
          })

#' @rdname cls
#' @export

setGeneric("clsRename", function(d,cls,newName, ...)
  standardGeneric("clsRename"))

#' @rdname cls

setMethod('clsRename',
          signature = 'AnalysisData',
          function(d,cls,newName){
            sinfo(d) <- d %>%
              sinfo() %>%
              rename(!!newName := !!cls)
            
            return(d)
          })

#' @rdname cls

setMethod('clsRename',
          signature = 'Analysis',
          function(d,cls,newName, type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'raw'){
              sl <- get('raw')  
              `sl<-` <- get(str_c('raw','<-'))
            } else {
              sl <- get('preTreated')
              `sl<-` <- get(str_c('preTreated','<-'))
            }
            
            sl(d) <- d %>%
              sl() %>%
              clsRename(cls = cls,newName = newName)
          })

#' @rdname cls
#' @export

setGeneric("clsReplace", function(d,value,cls = 'class', ...)
  standardGeneric("clsReplace"))

#' @rdname cls
#' @description Replace a given sample info column from an Analysis or 
#' AnalysisData object.

setMethod('clsReplace',
          signature = 'AnalysisData',
          function(d,value,cls = 'class'){
            if (!(cls %in% clsAvailable(d))) {
              stop(
                str_c('Class information column "',cls,'" not present.'),
                call. = FALSE)
            }
            
            sinfo(d)[,cls] <- value
            return(d)
          })

#' @rdname cls

setMethod('clsReplace',
          signature = 'Analysis',
          function(d, value, cls = 'class', type = c('raw','pre-treated')){
            type <- match.arg(type,
                              choices = c('raw',
                                          'pre-treated'))
            
            if (type == 'raw'){
              sl <- get('raw')  
              `sl<-` <- get(str_c('raw','<-'))
            } else {
              sl <- get('preTreated')
              `sl<-` <- get(str_c('preTreated','<-'))
            }
            
            sl(d) <- d %>%
              sl() %>%
              clsReplace(value = value,cls = cls)
            
            return(d)
          })
