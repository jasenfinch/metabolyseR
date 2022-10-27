#' Quality control (QC) sample treatments
#' @rdname QC
#' @description Quality control (QC) sample pre-treatment methods.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param occupancy occupancy threshold for filtering
#' @param RSDthresh RSD (%) threshold for filtering
#' @param parallel parallel type to use. See `?missForest` for details
#' @param seed random number seed
#' @return An S4 object of class `AnalysisData` containing QC treated data.
#' @details 
#' A QC sample is an average pooled sample, equally representative in composition of all the samples present within an experimental set.
#' Within an analytical run, the QC sample is analysed at equal intervals throughout the run.
#' If there is class structure within the run, this should be randomised within a block fashion so that the classes are equally represented in each block throughout the run.
#' A QC sample can then be injected and analysed between these randomised blocks.
#' This provides a set of technical injections that allows the variability in instrument performance over the run to be accounted for and the robustness of the acquired variables to be assessed.
#' 
#' The technical reproducibility of an acquired variable can be assessed using it's relative standard deviation (RSD) within the QC samples.
#' The variable RSDs can then be filtered below a threshold value to remove metabolome features that are poorly reproducible across the analytical runs.
#' This variable filtering strategy has an advantage over that of occupancy alone as it is not dependent on underlying class structure.
#' Therefore, the variables and variable numbers will not alter if a new class structure is imposed upon the data.
#' @section Methods:
#' * `QCimpute`: Missing value imputation of QC samples.
#' * `QCoccupancy`: Feature maximum occupancy filtering based on QC samples.
#' * `QCremove`: Remove QC samples.
#' * `QCrsdFilter`: Feature filtering based RSD of QC sample features.
#' @examples 
## The example data set does not contain QC samples so for the following example the 'H' class of the 'day' sample information column will be used.
#'
#' ## Initial example data preparation
#' library(metaboData)
#' d <- analysisData(abr1$neg[,1:1000],abr1$fact)
#'
#' ## Plot the feature RSD distributions of the H class only
#' d %>% 
#'  keepClasses(cls = 'day',classes = 'H') %>% 
#'  plotRSD(cls = 'day')
#'
#' ## Apply QC feature occupancy filtering and QC feature RSD filtering
#' QC_treated <- d %>% 
#'  QCoccupancy(cls = 'day',QCidx = 'H',occupancy = 2/3) %>%
#'  QCrsdFilter(cls = 'day',QCidx = 'H',RSDthresh = 50)
#'
#' print(QC_treated)
#'
#' ## Plot the feature RSD distributions of the H class after QC treatments
#' QC_treated %>% 
#'  keepClasses(cls = 'day',classes = 'H') %>% 
#'  plotRSD(cls = 'day')
#' @export

setGeneric("QCimpute", 
           function(
             d, 
             cls = 'class', 
             QCidx = 'QC', 
             occupancy = 2/3, 
             parallel = 'variables', 
             seed = 1234) 
             standardGeneric("QCimpute"))

#' @rdname QC
#' @importFrom missForest missForest
#' @importFrom utils capture.output

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
                     select(all_of(cls)) %>%
                     deframe() %>%
                     {. == QCidx},] <- QC %>%
              dat()
            return(d)
          }
)

#' @rdname QC
#' @export

setGeneric("QCoccupancy", 
           function(
             d,
             cls = 'class', 
             QCidx = 'QC', 
             occupancy = 2/3) 
             standardGeneric("QCoccupancy"))

#' @rdname QC
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

#' @rdname QC
#' @export

setGeneric("QCremove", function(d,cls = 'class', QCidx = 'QC')
  standardGeneric("QCremove"))

#' @rdname QC
#' @importFrom stats sd

setMethod('QCremove',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC'){
            d <- d %>%
              removeClasses(cls = cls,classes = QCidx)
            return(d)
          }
)

#' @rdname QC
#' @export

setGeneric("QCrsdFilter", 
           function(
             d,
             cls = 'class',
             QCidx = 'QC', 
             RSDthresh = 50) 
             standardGeneric("QCrsdFilter"))

#' @rdname QC
#' @importFrom stats sd

setMethod('QCrsdFilter',signature = 'AnalysisData',
          function(d,cls = 'class', QCidx = 'QC', RSDthresh = 50){
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
