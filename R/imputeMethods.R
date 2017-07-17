#' @importFrom missForest missForest
#' @importFrom parallel makeCluster stopCluster parLapply

imputeMethods <- function(method = NULL, description = F){
  
  methods <- list(
    
    all = function(dat, occupancy = 2/3){
      d <- as.matrix(dat$Data)
      d[d == 0] <- NA
      capture.output(d <- missForest(d))
      dat$Data <- as_tibble(d$ximp)
      return(dat)
    },
    
    class = function(dat, cls = 'class', idx = 'fileOrder', occupancy = 2/3, nCores = detectCores(), clusterType = 'FORK'){
      clus <- makeCluster(nCores)
      dat$Data <- parLapply(clus,as.character(sort(unique(unlist(dat$Info[,cls])))),function(y,dat,cls,occupancy,idx){
        d <- as.matrix(dat$Data)
        rownames(d) <- unlist(dat$Info[,idx])
        d <- d[unlist(dat$Info[,cls] == y),]
        occ <- occMat(d,rep(1,nrow(d)))
        dat.1 <- d[,occ < occupancy]
        d <- d[,!(occ < occupancy)]
        d[d == 0] <- NA
        capture.output(d <- missForest(d))
        d <- d$ximp
        d <- cbind(dat.1,d)
        d <- t(d)
        d <- d[order(as.numeric(str_replace_all(rownames(d),'[:alpha:]',''))),]
        d <- t(d)
        return(d)
      },dat = dat, cls = cls, occupancy = occupancy,idx = idx)
      stopCluster(clus)
      n <- unlist(lapply(dat$Data,rownames))
      dat$Data <- lapply(dat$Data,as_tibble) %>% bind_rows()
      dat$Data <- dat$Data[order(n),]
      return(dat)
    }
  )
  
  descriptions <- list(
    all = list(description = 'Impute missing values across all samples using Random Forest',
               arguments = c(occupancy = 'occupancy threshold for imputation')),
    class = list(description = 'Impute missing values class-wise using Random Forest',
                 arguments = c(cls = 'info column to use for class labels',
                               idx = 'info column to use for sample indexes',
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