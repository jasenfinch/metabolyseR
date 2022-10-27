
performRF <- function(x,cls,rf,type,returnModel){
  params <- formals(randomForest::randomForest)
  params$x <- x
  params$y <- cls
  params <- c(params,rf)
  model <- do.call(randomForest::randomForest,params)
  
  model_results <- list(metrics = performanceMetrics(model,
                                                     type = type),
                        importance = modelImportance(model,type),
                        predictions = modelPredictions(model,type),
                        proximities = modelProximities(model))
  
  if (isTRUE(returnModel)) model_results <- c(model_results,
                                              list(model = model))
  
  return(model_results)
}

performanceMetrics <- function(model,type){
  switch(type,
         classification = classificationMetrics(model),
         regression = regressionMetrics(model))
}

modelPredictions <- function(model,type){
  switch(type,
         classification = classificationPredictions(model),
         regression = regressionPredictions(model))
}

modelImportance <- function(model,type){
  switch(type,
         classification = classificationImportance(model),
         regression = model %>%
           randomForest::importance() %>%
           {bind_cols(tibble(feature = rownames(.)),as_tibble(.,.name_repair = 'minimal'))} %>% 
           gather(metric,value,-feature))
}

modelProximities <- function(model){
  model$proximity %>%
    as_tibble(.name_repair = 'minimal') %>%
    mutate(Sample = seq_len(nrow(.))) %>%
    gather('Sample2','Proximity',-Sample) %>%
    rename(Sample1 = Sample)
}

collate <- function(models,results,type){
  switch(type,
         classification = collateClassification(models,results),
         regression = collateRegression(models,results)
  )
}

#' @importFrom forestControl fpr_fs

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

supervised <- function(x,
                       cls,
                       rf,
                       reps,
                       binary,
                       comparisons,
                       perm,
                       returnModels,
                       seed){
  i <- x %>%
    sinfo() %>%
    select(all_of(cls))
  
  i %>%
    colnames() %>%
    map(~{
      cls <- .
      
      pred <- i %>%
        select(all_of(cls)) %>%
        deframe()
      
      if (is.numeric(pred)) {
        regression(x,
                   cls,
                   rf,
                   reps,
                   perm,
                   returnModels,
                   seed)
      } else {
        classification(x,
                       cls,
                       rf,
                       reps,
                       binary,
                       comparisons,
                       perm,
                       returnModels,
                       seed)
      }
    }) %>%
    set_names(colnames(i))
}