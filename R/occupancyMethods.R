
occupancyMethods <- function(method = NULL, description = F){
    
  methods <- list(
      maximum = function(dat,cls = 'class', occupancy = 2/3){
        occ <- occMat(dat,cls)
        fd <- occ %>%
          group_by(Feature) %>%
          summarise(Occupancy = max(Occupancy)) %>%
          filter(Occupancy >= occupancy)
        feat <- colnames(dat$Data)[colnames(dat$Data) %in% unique(fd$Feature)]
        dat$Data <- dat$Data %>%
          select(feat)
        return(dat)
      }, 
      minimum = function(dat,cls = 'class', occupancy = 2/3){
        occ <- occMat(dat,cls)
        fd <- occ %>%
          group_by(Feature) %>%
          summarise(Occupancy = min(Occupancy)) %>%
          filter(Occupancy >= occupancy)
        feat <- colnames(dat$Data)[colnames(dat$Data) %in% unique(fd$Feature)]
        dat$Data <- dat$Data %>%
          select(feat)
        return(dat)
      }
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

occMat <- function(dat,cls){
  d <- dat$Data %>%
    mutate(Class = unlist(dat$Info[,cls],use.names = F))
  
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
