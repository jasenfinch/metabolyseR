#' clsAvailable
#' @rdname clsAvailable
#' @description Return available sample info columns from an Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('clsAvailable',signature = 'AnalysisData',function(x){
  x %>%
    sinfo() %>%
    colnames()
})

#' @rdname clsAvailable

setMethod('clsAvailable',signature = 'Analysis',function(x,type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  
  x %>%
    sl() %>%
    clsAvailable()
})

#' clsExtract
#' @rdname clsExtract
#' @description Extract a given sample info column from an Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls sample info column to extract
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('clsExtract',signature = 'AnalysisData',function(x, cls = 'class'){
  x %>%
    sinfo() %>%
    select(cls) %>%
    deframe()
})

#' @rdname clsExtract

setMethod('clsExtract',signature = 'Analysis',function(x, cls = 'class', type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  
  x %>%
    sl() %>%
    clsExtract(cls = cls)
})

#' clsReplace
#' @rdname clsReplace
#' @description Replace a given sample info column from an Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param value vactor of new sample information for replacement
#' @param cls sample info column to replace
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('clsReplace',signature = 'AnalysisData',function(x,value,cls = 'class'){
  if (!(cls %in% clsAvailable(x))) {
    stop(str_c('Class information column "',cls,'" not present.'))
  }
  
  sinfo(x)[,cls] <- value
  return(x)
})

#' @rdname clsReplace

setMethod('clsReplace',signature = 'Analysis',function(x, value, cls = 'class', type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    clsReplace(value = value,cls = cls)
  
  return(x)
})

#' clsAdd
#' @rdname clsAdd
#' @description Add a sample info column to a Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls name of new sample information column
#' @param value new sample information to add
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @importFrom rlang :=
#' @export

setMethod('clsAdd',signature = 'AnalysisData',function(x,cls,value){
  if (cls %in% clsAvailable(x)) {
    stop(str_c('Class information column "',cls,'" already present.'))
  }
  
  sinfo(x) <- x %>%
    sinfo() %>%
    mutate({{cls}} := value)
  return(x)
})

#' @rdname clsAdd

setMethod('clsAdd',signature = 'Analysis',function(x,cls,value,type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    clsAdd(value = value,cls = cls)
  
  return(x)
})

#' clsRemove
#' @rdname clsRemove
#' @description Remove a sample info column from a Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls name of sample information column to remove
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('clsRemove',signature = 'AnalysisData',function(x,cls){
  if (!(cls %in% clsAvailable(x))) {
    stop(str_c('Class information column "',cls,'" not present.'))
  }
  
  sinfo(x) <- x %>%
    sinfo() %>%
    select(-{{cls}})
  
  return(x)
})

#' @rdname clsRemove

setMethod('clsRemove',signature = 'Analysis',function(x,cls,type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    clsRemove(cls)
  
  return(x)
})

#' clsArrange
#' @rdname clsArrange
#' @description Order samples within an object of class AnalysisData or Analysis by a given sample information column.
#' @param x S4 object of class AnalysisData or Analysis
#' @param cls  name of sample information column to arrange by
#' @param descending TRUE/FALSE, arrange samples in descending order
#' @param type  \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @importFrom dplyr desc
#' @export

setMethod('clsArrange',signature = 'AnalysisData',function(x,cls = 'class', descending = FALSE){
  
  sample_data <- dat(x) %>%
    bind_cols(sinfo(x) %>%
                select(all_of(cls)))
  
  sample_info <- sinfo(x)
  
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
  
  dat(x) <- sample_data %>%
    select(-all_of(cls))
  sinfo(x) <- sample_info
  
  return(x)
})

#' @rdname clsArrange

setMethod('clsArrange',signature = 'Analysis',function(x,cls = 'class', descending = FALSE, type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    clsArrange(cls = cls,descending = descending)
})

#' clsRename
#' @rdname clsRename
#' @description Rename a sample information column within an object of AnalysisData or Analysis.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls  sample information column to rename
#' @param newName new column name
#' @param type  \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('clsRename',signature = 'AnalysisData',function(x,cls,newName){
  sinfo(x) <- x %>%
    sinfo() %>%
    rename(!!newName := !!cls)
  
  return(x)
})

#' @rdname clsRename

setMethod('clsRename',signature = 'Analysis',function(x,cls,newName, type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    clsRename(cls = cls,newName = newName)
})