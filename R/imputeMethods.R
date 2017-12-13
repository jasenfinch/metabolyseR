#' @importFrom missForest missForest
#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom dplyr arrange

imputeMethods <- function(method = NULL, description = F){
  
  methods <- list(
    
    all = function(dat, occupancy = 2/3){
      d <- as.matrix(dat$Data)
      d[d == 0] <- NA
      set.seed(1234)
      capture.output(d <- missForest(d))
      dat$Data <- as_tibble(d$ximp)
      return(dat)
    },
    
    class = function(dat, cls = 'class', occupancy = 2/3, nCores = detectCores(), clusterType = 'FORK'){
      if (length(as.character(sort(unique(unlist(dat$Info[,cls]))))) < nCores) {
        nCores <- length(as.character(sort(unique(unlist(dat$Info[,cls])))))
      }
      
      classes <- dat$Info %>%
        select(Class = cls)
      
      dat$Data <- dat$Data %>%
        rowid_to_column(var = 'ID') %>%
        bind_cols(classes)
      
      dat$Data <- dat$Data %>%
        split(.$Class)
      
      clus <- makeCluster(nCores,type = clusterType)

      dat$Data <- parLapply(clus,dat$Data,function(d,occupancy){
        idx <- d %>% 
          select(ID,Class)
        
        occ <- d %>%
          gather('Feature','Intensity',-ID,-Class) %>%
          group_by(Class,Feature) %>%
          filter(Intensity > 0) %>%
          summarise(Count = n()) %>%
          mutate(Occupancy = Count/nrow(d)) %>%
          filter(Occupancy >= occupancy)
        
        lowOcc <- d %>%
          select(-ID,-Class)
        lowOcc <- lowOcc[,!(colnames(lowOcc) %in% occ$Feature)]
        
        d <- d %>%
          select(colnames(d)[colnames(d) %in% occ$Feature])
        
        x <- d %>%
          rowid_to_column(var = 'ID') %>%
          gather('Feature','Intensity',-ID)
        
        if (0 %in% x$Intensity) {
          set.seed(1234)
          capture.output(d <- missForest(d))
          d <- d$ximp %>%
            as_tibble()
        }
        
        d <- d %>%
          bind_cols(lowOcc,idx)
        
        return(d)
      },occupancy = occupancy)
      stopCluster(clus)
      
      dat$Data <- dat$Data %>%
        bind_rows() %>%
        arrange(ID) %>%
        select(-ID,-Class)
      return(dat)
    }
  )
  
  descriptions <- list(
    all = list(description = 'Impute missing values across all samples using Random Forest',
               arguments = c(occupancy = 'occupancy threshold for imputation')),
    class = list(description = 'Impute missing values class-wise using Random Forest',
                 arguments = c(cls = 'info column to use for class labels',
                               occupancy = 'occupancy threshold for imputation', 
                               nCores = 'number of cores for parallisation',
                               clusterType = 'cluster type for parallisation'))
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