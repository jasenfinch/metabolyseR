#' @importFrom missForest missForest
#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom dplyr arrange

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
      if (length(as.character(sort(unique(unlist(dat$Info[,cls]))))) < nCores) {
        nCores <- length(as.character(sort(unique(unlist(dat$Info[,cls])))))
      }
      clus <- makeCluster(nCores,type = clusterType)
      dat$Data <- parLapply(clus,as.character(sort(unique(unlist(dat$Info[,cls])))),function(y,dat,cls,occupancy,idx){
        d <- as.matrix(dat$Data)
        d <- d[unlist(dat$Info[,cls] == y),]
        rowIdx <- unlist(dat$Info[unlist(dat$Info[,cls] == y),idx])
        occ <- occMat(d,rep(1,nrow(d)))
        dat.1 <- d[,occ < occupancy]
        d <- d[,!(occ < occupancy)]
        d[d == 0] <- NA
        capture.output(d <- missForest(d))
        d <- d$ximp
        d <- cbind(dat.1,d)
        d <- t(d)
        d <- d[order(as.numeric(str_replace_all(rownames(d),'[:alpha:]',''))),]
        d <- t(d) %>%
          as_tibble() %>%
          bind_cols(rowIdx = rowIdx)
        return(d)
      },dat = dat, cls = cls, occupancy = occupancy,idx = idx)
      stopCluster(clus)
      
      dat$Data <- dat$Data %>%
        bind_rows() %>%
        arrange(rowIdx) %>%
        select(-rowIdx)
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