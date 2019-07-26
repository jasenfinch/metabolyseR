#' correctionCenter
#' @rdname correctionCenter
#' @description Batch correction using average centering.
#' @param d S4 object of class AnalysisData
#' @param block info column containing sample block groupings to use for correction
#' @param type averaging to use; eg. mean or median
#' @export

setMethod('correctionCenter',signature = 'AnalysisData',
          function(d, block = 'block', type = 'median'){
            method <- get(type)
            batches <- sinfo(d)[,block] %>% unlist()
            dat(d) <- dat(d) %>%
              map_df(~{
                da <- .
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
              })
            return(d)
          }
)

correctionMethods <- function(method = NULL, description = F){
  methods <- list(
    center = correctionCenter
  )
  
  descriptions <- list(
    center = list(
      description = 'Batch correction using average centering.',
      arguments = c(block = 'info column containing sample block groupings to use for correction',
                    type = 'averaging to use; eg. mean or median'
      )
    )
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