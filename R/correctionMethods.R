
correctionMethods <- function(method = NULL, description = F){
  methods <- list(
    center = function(dat, cls = 'block', type = 'median'){
      method <- get(type)
      batches <- dat$Info[,cls] %>% unlist()
      dat$Data <- dat$Data %>%
        map_df(~{
          d <- .
          batchMeans <- tibble(Intensity = d,batch = batches) %>%
            group_by(batch) %>%
            mutate(Intensity = Intensity) %>%
            summarise(Median = method(Intensity)) %>%
            mutate(correction = Median - method(Median))
          
          correct <- tibble(Intensity = d,batch = batches) %>%
            left_join(batchMeans, by = "batch") %>%
            mutate(Intensity = Intensity) %>%
            mutate(Intensity = Intensity - correction) %>%
            dplyr::select(-correction,-Median,-batch) %>%
            unlist()
          correct[correct < 0] <- 0
          return(correct)
        })
      return(dat)
    }
  )
  
  descriptions <- list()
  
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