#' imputeAll
#' @rdname imputeAll
#' @description Impute missing values across all samples using Random Forest.
#' @param d S4 object of class AnalysisData
#' @param occupancy occupancy threshold for imputation of a given feature
#' @param parallel parallel type to use. See `?missForest` for details
#' @param nCores number of cores for parallisation
#' @param clusterType cluster type for parallisation
#' @param seed random number seed
#' @importFrom tidyselect all_of
#' @export

setMethod('imputeAll',signature = 'AnalysisData',
          function(d, 
                   occupancy = 2/3, 
                   parallel = 'variables', 
                   seed = 1234){
            
            d <- clsAdd(d,cls = 'dummy',rep(1,nSamples(d)))
            
            occ <- occupancy(d,cls = 'dummy')
            
            low_occ <- occ %>%
              filter(Occupancy < occupancy) %>%
              select(Feature) %>%
              distinct() %>%
              deframe()
            
            d_low_occ <- d %>%
              keepFeatures(low_occ)
            
            d_to_impute <- d %>%
              removeFeatures(low_occ)
            
            set.seed(seed)
            
            da <- d_to_impute %>%
              dat() %>%
              as.matrix()
            
            da[da == 0] <- NA
            
            capture.output({
              da <- missForest(da,parallelize = parallel)
            })  
            
            dat(d_to_impute) <- as_tibble(da$ximp)
            
            feat <- features(d)
            
            dat(d) <- bind_cols(dat(d_to_impute),dat(d_low_occ)) %>%
              select(all_of(feat))
            
            d <- clsRemove(d,cls = 'dummy')
            
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
          function(d, 
                   cls = 'class', 
                   occupancy = 2/3, 
                   seed = 1234)
          {
            
            d <- d %>%
              clsAdd(cls = 'dummy_ind',1:nSamples(d))
            
            ind_classes <- d %>% 
              clsExtract(cls) %>% 
              unique()
            
            d <- ind_classes %>%
              future_map(~{
                
                d %>%
                  keepClasses(cls = cls,classes = .x) %>%
                  imputeAll(occupancy = occupancy,seed = seed)
              },d = d,
              cls = cls,
              occupancy = occupancy,
              seed = seed)
            
            d <- d %>%
              bindAnalysesRows() %>%
              clsArrange(cls = 'dummy_ind') %>%
              clsRemove(cls = 'dummy_ind')
            
            return(d)
          }
)


#' @importFrom missForest missForest
#' @importFrom dplyr arrange select
#' @importFrom doParallel registerDoParallel

imputeMethods <- function(method = NULL, description = FALSE){
  
  methods <- list(
    
    all = imputeAll,
    
    class = imputeClass
  )
  
  descriptions <- list(
    all = list(
      description = str_c('Impute missing values across all ',
                          'samples using Random Forest'),
      arguments = c(
        occupancy = 'occupancy threshold for imputation',
        parallel = str_c('parallel type to use. ',
                         'See `?missForest` for details'),
        seed = 'random number seed'
      )),
    class = list(
      description = 'Impute missing values class-wise using Random Forest',
      arguments = c(
        cls = 'info column to use for class labels',
        occupancy = 'occupancy threshold for imputation', 
        seed = 'random number seed'))
  )
  
  if (description == FALSE) {
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
