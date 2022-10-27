
unsupervised <- function(x,rf,reps,returnModels,seed,...){
  
  set.seed(seed)
  
  models <- future_map(1:reps,~{
    params <- formals(randomForest::randomForest)
    params$x <- x %>% dat()
    params <- c(params,rf)
    do.call(randomForest::randomForest,params)
  },.options = furrr_options(seed = seed)) %>%
    set_names(1:reps)
  
  importances <- models %>%
    future_map_dfr(~{
      m <- .
      importance(m) %>%
        left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
        rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
    },
    .id = 'Rep',
    .options = furrr_options(seed = seed)) %>%
    mutate(Rep = as.numeric(Rep)) %>%
    gather('Metric','Value',-Rep,-Feature)
  
  proximities <- models %>%
    future_map_dfr(.,~{.$proximity %>%
        as_tibble(.name_repair = 'minimal') %>%
        mutate(Sample = seq_len(nrow(.))) %>%
        gather('Sample2','Proximity',-Sample) %>%
        rename(Sample1 = Sample)
    },
    .id = 'Rep',
    .options = furrr_options(seed = seed)) %>%
    mutate(Rep = as.numeric(Rep))  %>%
    mutate(Sample2 = as.numeric(Sample2))
  
  results <- list(
    importances = importances %>%
      select(-Rep) %>%
      group_by(Feature,Metric) %>%
      summarise(Value = mean(Value))
  )
  
  res <- new('RandomForest')
  res@type <- 'unsupervised'
  dat(res) <- dat(x)
  sinfo(res) <- sinfo(x)
  res@results <- results
  res@importances <- importances
  res@proximities <- proximities
  
  if (isTRUE(returnModels)) {
    res@models <- models
  }
  
  return(list(res))  
}