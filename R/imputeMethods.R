#' imputeAll
#' @rdname imputeAll
#' @description Impute missing values across all samples using Random Forest.
#' @param d S4 object of class AnalysisData
#' @param occupancy occupancy threshold for imputation
#' @param parallel parallel type to use. See `?missForest` for details
#' @param nCores number of cores for parallisation
#' @param clusterType cluster type for parallisation
#' @param seed random number seed
#' @export

setMethod('imputeAll',signature = 'AnalysisData',
          function(d, occupancy = 2/3, parallel = 'variables', nCores = detectCores() * 0.75, clusterType = getClusterType(), seed = 1234){
            
            set.seed(seed)
            da <- as.matrix(d %>% dat())
            da[da == 0] <- NA
            if (nCores > 1) {
              cl <- makeCluster(nCores,type = clusterType)
              registerDoParallel(cl)
              capture.output(da <- missForest(da,parallelize = parallel))  
              stopCluster(cl)
            } else {
              capture.output(da <- missForest(da))  
            }
            
            dat(d) <- as_tibble(da$ximp)
            return(d)
          }
)

#' imputeClass
#' @rdname imputeClass
#' @description Impute missing values class-wise using Random Forest.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param occupancy occupancy threshold for imputation
#' @param nCores number of cores for parallisation
#' @param clusterType cluster type for parallisation
#' @param seed random number seed
#' @importFrom dplyr n
#' @export

setMethod('imputeClass',signature = 'AnalysisData',
          function(d, cls = 'class', occupancy = 2/3, nCores = detectCores() * 0.75, clusterType = getClusterType(), seed = 1234){
            set.seed(seed)
            if (length(as.character(sort(unique(unlist(sinfo(d)[,cls]))))) < nCores) {
              nCores <- length(as.character(sort(unique(unlist(sinfo(d)[,cls])))))
            }
            
            classes <- sinfo(d) %>%
              select(Class = cls)
            
            dat(d) <- dat(d) %>%
              rowid_to_column(var = 'ID') %>%
              bind_cols(classes)
            
            da <- dat(d) %>%
              split(.$Class)
            
            clus <- makeCluster(nCores,type = clusterType)
            
            da <- parLapply(clus,da,function(da,occupancy){
              idx <- da %>% 
                select(ID,Class)
              
              occ <- da %>%
                gather('Feature','Intensity',-ID,-Class) %>%
                group_by(Class,Feature) %>%
                filter(Intensity > 0) %>%
                summarise(Count = n()) %>%
                mutate(Occupancy = Count/nrow(da)) %>%
                filter(Occupancy >= occupancy)
              
              lowOcc <- da %>%
                select(-ID,-Class)
              lowOcc <- lowOcc[,!(colnames(lowOcc) %in% occ$Feature)]
              
              da <- da %>%
                select(colnames(da)[colnames(da) %in% occ$Feature])
              
              x <- da %>%
                rowid_to_column(var = 'ID') %>%
                gather('Feature','Intensity',-ID)
              
              if (0 %in% x$Intensity) {
                capture.output(da <- missForest(da))
                da <- da$ximp %>%
                  as_tibble()
              }
              
              da <- da %>%
                bind_cols(lowOcc,idx)
              
              return(da)
            },occupancy = occupancy)
            stopCluster(clus)
            
            dat(d) <- dat(d) %>%
              bind_rows() %>%
              arrange(ID) %>%
              select(-ID,-Class)
            return(d)
          }
          )


#' @importFrom missForest missForest
#' @importFrom parallel makeCluster stopCluster parLapply
#' @importFrom dplyr arrange select
#' @importFrom doParallel registerDoParallel

imputeMethods <- function(method = NULL, description = F){
  
  methods <- list(
    
    all = imputeAll,
    
    class = imputeClass
  )
  
  descriptions <- list(
    all = list(description = 'Impute missing values across all samples using Random Forest',
               arguments = c(occupancy = 'occupancy threshold for imputation',
                             parallel = 'parallel type to use. See `?missForest` for details',
                             nCores = 'number of cores for parallisation',
                             clusterType = 'cluster type for parallisation',
                             seed = 'random number seed'
               )),
    class = list(description = 'Impute missing values class-wise using Random Forest',
                 arguments = c(cls = 'info column to use for class labels',
                               occupancy = 'occupancy threshold for imputation', 
                               nCores = 'number of cores for parallisation',
                               clusterType = 'cluster type for parallisation',
                               seed = 'random number seed'))
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Impute method '",
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
        stop(str_c("Impute method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}