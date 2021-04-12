#' QCoccupancy
#' @rdname QCoccupancy
#' @description QC maximum occupancy filter.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param occupancy occupancy threshold for filtering
#' @export

setMethod('QCoccupancy',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC', occupancy = 2/3){
            method <- occupancyMethods('maximum')
            QC <- d %>%
              keepClasses(cls = cls,classes = QCidx)
            
            QC <- method(QC,cls,occupancy)
            d <- d %>% 
              keepFeatures(features = features(QC))
            return(d)
          }
)

#' QCimpute
#' @rdname QCimpute
#' @description QC imputation.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param occupancy occupancy threshold for imputation
#' @param parallel parallel type to use. See `?missForest` for details
#' @param seed random number seed
#' @importFrom missForest missForest
#' @importFrom utils capture.output
#' @export

setMethod('QCimpute',signature = 'AnalysisData',
          function(d, 
                   cls = 'class', 
                   QCidx = 'QC', 
                   occupancy = 2/3, 
                   parallel = 'variables', 
                   seed = 1234){
            set.seed(seed)
            QC <-  d %>%
              keepClasses(cls = cls,classes = QCidx) %>%
              imputeAll(occupancy = occupancy,
                        parallel = parallel,
                        seed = seed)
            
            dat(d)[d %>% 
                     sinfo() %>% 
                     select(cls) %>%
                     deframe() %>%
                     {. == QCidx},] <- QC %>%
              dat()
            return(d)
          }
)

#' QCrsdFilter
#' @rdname QCrsdFilter
#' @description QC relative standard deviation (RSD) filtering..
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param RSDthresh RSD threshold for filtering
#' @importFrom stats sd
#' @export

setMethod('QCrsdFilter',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC', RSDthresh = 0.5){
            QC <- d %>%
              keepClasses(cls = cls,classes = QCidx)
            
            RSD <- QC %>%
              rsd(cls = cls) %>%
              filter(RSD < RSDthresh)
            
            d <- d %>%
              keepFeatures(features = RSD$Feature)
            
            return(d)
          }
)

#' QCremove
#' @rdname QCremove
#' @description Remove QC samples.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @importFrom stats sd
#' @export

setMethod('QCremove',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC'){
            d <- d %>%
              removeClasses(cls = cls,classes = QCidx)
            return(d)
          }
)

QCMethods <- function(method = NULL, description = FALSE){
  
  methods <- list(
    occupancyFilter = QCoccupancy,
    impute = QCimpute,
    RSDfilter = QCrsdFilter,
    removeQC = QCremove
  ) 
  
  descriptions <- list(
    occupancyFilter = list(
      description = 'Filter variables based on occupancy in QC samples',
      arguments = c(cls = 'info column to use for class labels',
                    QCidx = 'QC sample label',
                    occupancy = 'occupancy threshold for filtering')),
    impute = list(
      description = 'Impute missing values in QC samples',
      arguments = c(cls = 'info column to use for class labels',
                    QCidx = 'QC sample label',
                    occupancy = 'occupancy threshold for imputation',
                    parallel = str_c('parallel type to use. See ',
                                     '`?missForest` for details'),
                    seed = 'random number seed')
    ),
    RSDfilter = list(
      description = str_c('Filter features based on their ',
                          'relative standard deviation in QC samples'),
      arguments = c(cls = 'info column to use for class labels',
                    QCidx = 'QC sample label',
                    RSDthreshold = 'RSD threshold for filtering')),
    removeQC = list(
      description = 'Remove QC samples',
      arguments = c(cls = 'info column to use for class labels',
                    QCidx = 'QC sample label'))
  )
  
  if (description == FALSE) {
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("QC method '",
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
        stop(str_c("QC method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}

#' rsd
#' @rdname rsd
#' @description Calculate relative standard deviation values for each 
#' feature per class for a given info column.
#' @param x S4 object of class AnalysisData
#' @param cls info column to use for class structure
#' @export

setMethod('rsd',signature = 'AnalysisData',
          function(x,cls = 'class'){
            vars <- 'Class'
            names(vars) <- cls
            
            x %>%
              dat() %>%
              mutate(Class = clsExtract(x,cls)) %>%
              gather(Feature,Intensity,-Class) %>%
              group_by(Class,Feature) %>%
              summarise(RSD = sd(Intensity)/mean(Intensity)) %>%
              rename(!!vars)
          })
