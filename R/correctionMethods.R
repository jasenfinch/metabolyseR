#' correctionCenter
#' @rdname correctionCenter
#' @description Batch correction using average centering.
#' @param d S4 object of class AnalysisData
#' @param block info column containing sample block 
#' groupings to use for correction
#' @param type averaging to use; eg. mean or median
#' @param nCores number of cores for parallisation
#' @param clusterType cluster type for parallisation
#' @export

setMethod('correctionCenter',signature = 'AnalysisData',
          function(d, 
                   block = 'block', 
                   type = 'median', 
                   nCores = detectCores() * 0.75, 
                   clusterType = getClusterType())
          {
            method <- get(type)
            batches <- sinfo(d)[,block] %>% unlist()
            
            clus <- makeCluster(nCores,type = clusterType)
            
            dat(d) <- dat(d) %>%
              parLapply(clus,.,function(da,batches,method){
                batchMeans <- tibble(Intensity = da,batch = batches) %>%
                  group_by(batch) %>%
                  mutate(Intensity = Intensity) %>%
                  summarise(Median = method(Intensity)) %>%
                  mutate(correction = Median - method(Median))
                
                correct <- tibble(Intensity = da,batch = batches) %>%
                  left_join(batchMeans, by = "batch") %>%
                  mutate(Intensity = Intensity) %>%
                  mutate(Intensity = Intensity - correction) %>%
                  dplyr::select(-correction,-Median,-batch) %>%
                  unlist()
                correct[correct < 0] <- 0
                return(correct)
              },batches = batches,method = method) %>%
              as_tibble()
            
            stopCluster(clus)
            
            return(d)
          }
)

correctionMethods <- function(method = NULL, description = FALSE){
  methods <- list(
    center = correctionCenter
  )
  
  descriptions <- list(
    center = list(
      description = 'Batch correction using average centering.',
      arguments = c(block = str_c('info column containing sample block ',
                                  'groupings to use for correction'),
                    type = 'averaging to use; eg. mean or median',
                    nCores = 'number of cores for parallisation',
                    clusterType = 'cluster type for parallisation'
      )
    )
  )
  
  if (description == FALSE) {
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Correction method '",
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
        stop(str_c("Correction method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}
