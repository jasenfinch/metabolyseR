#' availableCls
#' @rdname availableCls
#' @description Return available sample info columns from an Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('availableCls',signature = 'AnalysisData',function(x){
  x %>%
    sinfo() %>%
    colnames()
})

#' @rdname availableCls

setMethod('availableCls',signature = 'Analysis',function(x,type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  
  x %>%
    sl() %>%
    availableCls()
})

#' extractCls
#' @rdname extractCls
#' @description Extract a given sample info column from an Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls sample info column to extract
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('extractCls',signature = 'AnalysisData',function(x, cls = 'class'){
  x %>%
    sinfo() %>%
    select(cls) %>%
    deframe()
})

#' @rdname extractCls

setMethod('extractCls',signature = 'Analysis',function(x, cls = 'class', type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  
  x %>%
    sl() %>%
    extractCls(cls = cls)
})

#' replaceCls
#' @rdname replaceCls
#' @description Replace a given sample info column from an Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param value vactor of new sample information for replacement
#' @param cls sample info column to replace
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('replaceCls',signature = 'AnalysisData',function(x,value,cls = 'class'){
  if (!(cls %in% availableCls(x))) {
    stop(str_c('Class information column "',cls,'" not present.'))
  }
  
  sinfo(x)[,cls] <- value
  return(x)
})

#' @rdname replaceCls

setMethod('replaceCls',signature = 'Analysis',function(x, value, cls = 'class', type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    replaceCls(value = value,cls = cls)
  
  return(x)
})

#' addCls
#' @rdname addCls
#' @description Add a sample info column to a Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls name of new sample information column
#' @param value new sample information to add
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @importFrom rlang :=
#' @export

setMethod('addCls',signature = 'AnalysisData',function(x,cls,value){
  if (cls %in% availableCls(x)) {
    stop(str_c('Class information column "',cls,'" already present.'))
  }
  
  sinfo(x) <- x %>%
    sinfo() %>%
    mutate({{cls}} := value)
  return(x)
})

#' @rdname addCls

setMethod('addCls',signature = 'Analysis',function(x,cls,value,type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    addCls(value = value,cls = cls)
  
  return(x)
})

#' removeCls
#' @rdname removeCls
#' @description Remove a sample info column from a Analysis or AnalysisData object.
#' @param x S4 object of class Analysis or AnalysisData
#' @param cls name of sample information column to remove
#' @param type \code{raw} or \code{preTreated} sample information
#' @param ... arguments to pass to specific method
#' @export

setMethod('removeCls',signature = 'AnalysisData',function(x,cls){
  if (!(cls %in% availableCls(x))) {
    stop(str_c('Class information column "',cls,'" not present.'))
  }
  
  sinfo(x) <- x %>%
    sinfo() %>%
    select(-{{cls}})
  
  return(x)
})

#' @rdname removeCls

setMethod('removeCls',signature = 'Analysis',function(x,cls,type = 'raw'){
  types <- c('raw','preTreated')
  if (!(type %in% types)) {
    stop(str_c('Type should be one of ',str_c(str_c('"',types,'"'),collapse = ' or ')))
  } 
  
  sl <- get(type)
  `sl<-` <- get(str_c(type,'<-'))
  
  sl(x) <- x %>%
    sl() %>%
    removeCls(cls)
  
  return(x)
})