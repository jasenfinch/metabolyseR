#' Batch/block correction
#' @rdname  correction
#' @description Correction of batch/block differences.
#' @param d S4 object of class `AnalysisData`
#' @param block sample information column name to use containing sample block 
#' groupings
#' @param type type of average to use
#' @return An S4 object of class `AnalysisData` containing the corrected data. 
#' @details 
#' There can sometimes be artificial batch related variability introduced into metabolomics analyses as a result of analytical instrumentation or sample preparation. 
#' With an appropriate randomised block design of sample injection order, batch related variability can be corrected using an average centring correction method of the individual features.
#' @section Methods:
#' * `correctionCenter`: Correction using group average centring.
#' @examples 
#' 
#' ## Initial example data preparation
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact) %>% 
#'  occupancyMaximum(occupancy = 2/3)
#'  
#' ## Group total ion count distributions prior to correction
#' d %>% 
#'  plotTIC(by = 'day',colour = 'day')
#'  
#' ## Group total ion count distributions after group median correction
#' d %>% 
#'  correctionCenter(block = 'day',type = 'median') %>% 
#'  plotTIC(by = 'day',colour = 'day')
#' @export

setGeneric("correctionCenter", 
           function(
             d, 
             block = 'block', 
             type = c('mean','median')) 
             standardGeneric("correctionCenter"))

#' @rdname correction
#' @importFrom furrr future_map 

setMethod('correctionCenter',signature = 'AnalysisData',
          function(d, 
                   block = 'block', 
                   type = c('mean','median'))
          {
            
            type <- match.arg(type,choices = c('mean',
                                               'median'))
            
            method <- get(type)
            batches <- sinfo(d)[,block] %>% unlist()
            
            dat(d) <- dat(d) %>%
              future_map(~{
                batchMeans <- tibble(Intensity = .x,batch = batches) %>%
                  group_by(batch) %>%
                  mutate(Intensity = Intensity) %>%
                  summarise(Median = method(Intensity)) %>%
                  mutate(correction = Median - method(Median))
                
                correct <- tibble(Intensity = .x,batch = batches) %>%
                  left_join(batchMeans, by = "batch") %>%
                  mutate(Intensity = Intensity) %>%
                  mutate(Intensity = Intensity - correction) %>%
                  select(-correction,-Median,-batch) %>%
                  unlist()
                correct[correct < 0] <- 0
                return(correct)
              },batches = batches,method = method) %>%
              as_tibble()
            
            return(d)
          }
)
