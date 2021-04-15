#' Missing data imputation
#' @rdname  impute
#' @description Impute missing values using random forest imputation.
#' @param d S4 object of class `AnalysisData`
#' @param cls info column to use for class labels
#' @param occupancy occupancy threshold above which missing values of a feature will be imputed
#' @param parallel parallel type to use. See `?missForest` for details
#' @param seed random number seed
#' @return An S4 object of class `AnalysisData` containing the data after imputation.
#' @details 
#' Missing values can have an important influence on downstream analyses with zero values heavily influencing the outcomes of parametric tests. 
#' Where and how they are imputed are important considerations and is highly related to variable occupancy. 
#' The methods provided here allow both these aspects to be taken into account and utilise random forest imputation using the `missForest` package.
#' @section Methods:
#' * `imputeAll`: Impute missing values across all sample features.
#' * `imputeClass`: Impute missing values class-wise.
#' @examples 
#' ## Each of the following examples shows the application of each imputation method and then 
#' ## a Linear Discriminant Analysis is plotted to show it's effect on the data structure.
#' 
#' ## Initial example data preparation
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact) %>% 
#'  occupancyMaximum(occupancy = 2/3)
#' 
#' d %>% 
#'  plotLDA(cls = 'day')
#'  
#' ## Missing value imputation across all samples
#' d %>% 
#'  imputeAll(parallel = 'no') %>% 
#'  plotLDA(cls = 'day')
#' 
#' ## Missing value imputation class-wise
#' d %>% 
#'  imputeClass(cls = 'day') %>% 
#'  plotLDA(cls = 'day')
#' @export

setGeneric("imputeAll", 
           function(
             d, 
             occupancy = 2/3, 
             parallel = 'variables', 
             seed = 1234) 
           {
             standardGeneric("imputeAll")
           })

#' @rdname impute
#' @importFrom missForest missForest
#' @importFrom dplyr select
#' @importFrom tidyselect all_of
#' @importFrom doFuture registerDoFuture

setMethod('imputeAll',signature = 'AnalysisData',
          function(d, 
                   occupancy = 2/3, 
                   parallel = 'variables', 
                   seed = 1234){
            
            registerDoFuture()
            
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
            
            suppressWarnings({
              capture.output({
                da <- missForest(da,parallelize = parallel)
              })  
            })
            
            dat(d_to_impute) <- as_tibble(da$ximp)
            
            feat <- features(d)
            
            dat(d) <- bind_cols(dat(d_to_impute),dat(d_low_occ)) %>%
              select(all_of(feat))
            
            d <- clsRemove(d,cls = 'dummy')
            
            return(d)
          }
)

#' @rdname  impute
#' @export

setGeneric("imputeClass", 
           function(
             d, 
             cls = 'class', 
             occupancy = 2/3, 
             seed = 1234) 
           {
             standardGeneric("imputeClass")
           })

#' @rdname impute
#' @importFrom furrr furrr_options

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
                  imputeAll(occupancy = occupancy,seed = seed,parallel = 'no')
              },
              .options = furrr_options(seed = seed))
            
            d <- d %>%
              bindAnalysesRows() %>%
              clsArrange(cls = 'dummy_ind') %>%
              clsRemove(cls = 'dummy_ind')
            
            return(d)
          }
)
