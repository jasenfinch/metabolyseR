#' @importFrom dplyr tbl_df arrange_

aggregateMethods <- function(method = NULL, description = F){
  
  methods <- list(
    sum = function(dat, cls = 'class'){
      dat$Data <- dat$Data %>%
        bind_cols(select(dat$Info,Class = cls)) %>%
        gather('Feature','Intensity',-Class) %>%
        group_by(Class,Feature) %>%
        summarise(Intensity = sum(Intensity)) %>%
        tbl_df() %>%
        spread(Feature,Intensity) %>%
        select(-Class)
      
      dat$Info <- dat$Info %>% 
        select(cls) %>%
        unique() %>%
        arrange_(cls)
      
      return(dat)
    },
    mean = function(dat, cls = 'class'){
      dat$Data <- dat$Data %>%
        bind_cols(select(dat$Info,Class = cls)) %>%
        gather('Feature','Intensity',-Class) %>%
        group_by(Class,Feature) %>%
        summarise(Intensity = mean(Intensity)) %>%
        tbl_df() %>%
        spread(Feature,Intensity) %>%
        select(-Class)
      
      dat$Info <- dat$Info %>% 
        select(cls) %>%
        unique() %>%
        arrange_(cls)
      
      return(dat)
    } ,
    median = function(dat, cls = 'class'){
      dat$Data <- dat$Data %>%
        bind_cols(select(dat$Info,Class = cls)) %>%
        gather('Feature','Intensity',-Class) %>%
        group_by(Class,Feature) %>%
        summarise(Intensity = median(Intensity)) %>%
        tbl_df() %>%
        spread(Feature,Intensity) %>%
        select(-Class)
      
      dat$Info <- dat$Info %>% 
        select(cls) %>%
        unique() %>%
        arrange_(cls)
      
      return(dat)
    }
  )
  
  descriptions = list(
    sum = list(description = 'sum aggregate',
              arguments = c(cls = 'info column to use for aggregation index')),
    mean = list(description = 'mean aggregate',
                arguments = c(cls = 'info column to use for aggregation index')),
    median = list(description = 'median aggregate',
                  arguments = c(cls = 'info column to use for aggregation index'))
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      method <- descriptions[[method]]
    }
  }
  return(method)
}