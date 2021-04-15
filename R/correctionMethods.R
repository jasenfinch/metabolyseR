#' correctionCenter
#' @rdname correctionCenter
#' @description Batch correction using average centering.
#' @param d S4 object of class AnalysisData
#' @param block info column containing sample block 
#' groupings to use for correction
#' @param type averaging to use; eg. mean or median
#' @importFrom furrr future_map 
#' @export

setMethod('correctionCenter',signature = 'AnalysisData',
          function(d, 
                   block = 'block', 
                   type = 'median')
          {
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

correctionMethods <- function(method = NULL){
  methods <- list(
    center = correctionCenter
  )
  
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
  
  return(method)
}
