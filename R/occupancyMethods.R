#' occupancyMaximum
#' @rdname occupancyMaximum
#' @description Maximum occupancy filtering of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @param occupancy occupancy threshold
#' @export

setMethod('occupancyMaximum',signature = 'AnalysisData',
          function(dat,cls = 'class', occupancy = 2/3){
            occ <- occMat(dat,cls)
            fd <- occ %>%
              group_by(Feature) %>%
              summarise(Occupancy = max(Occupancy)) %>%
              filter(Occupancy >= occupancy)
            feat <- colnames(dat %>% dat)[colnames(dat %>% dat) %in% unique(fd$Feature)]
            dat@data <- dat %>%
              dat %>%
              select(feat)
            return(dat)
          }
)

#' occupancyMinimum
#' @rdname occupancyMinimum
#' @description Minimum occupancy filtering of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @param occupancy occupancy threshold
#' @export

setMethod('occupancyMinimum',signature = 'AnalysisData',
          function(dat,cls = 'class', occupancy = 2/3){
            occ <- occMat(dat,cls)
            fd <- occ %>%
              group_by(Feature) %>%
              summarise(Occupancy = min(Occupancy)) %>%
              filter(Occupancy >= occupancy)
            feat <- colnames(dat %>% dat())[colnames(dat %>% dat()) %in% unique(fd$Feature)]
            dat@data <- dat@data %>%
              select(feat)
            return(dat)
          }
)

occupancyMethods <- function(method = NULL, description = F){
  
  methods <- list(
    maximum = occupancyMaximum, 
    minimum = occupancyMinimum
  )
  
  descriptions = list(
    maximum = list(description = 'maximum thresholded class occupancy filtering', 
                   arguments = c(cls = 'info column to use for class labels', 
                                 occupancy = 'occupancy threshold')),
    minimum = list(description = 'minimum thresholded class occupancy filtering', 
                   arguments = c(cls = 'info column to use for class labels', 
                                 occupancy = 'occupancy threshold'))
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Occupancy method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Occupancy method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}

occMat <- function(dat,cls){
  d <- dat %>%
    dat() %>%
    mutate(Class = unlist(dat %>% sinfo() %>% .[,cls],use.names = F))
  
  clsSize <- d %>%
    group_by(Class) %>%
    summarise(Frequency = n())
  
  d <- d %>%
    rowid_to_column(var = 'Sample') %>%
    gather('Feature','Intensity',-Class,-Sample) %>%
    filter(Intensity > 0)
  
  occ <- clsSize %>%
    split(1:nrow(.)) %>%
    map(~{
      cla <- .
      cl <- d %>%
        filter(Class == cla$Class) %>%
        group_by(Class,Feature) %>%
        summarise(N = n()) %>%
        mutate(Occupancy = N / cla$Frequency)
    }) %>%
    bind_rows()
  return(occ)
}
