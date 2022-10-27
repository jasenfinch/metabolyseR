
performRF <- function(x,cls,rf,type,returnModel){
  params <- formals(randomForest::randomForest)
  params$x <- x
  params <- c(params,rf)
  
  if (!is.null(cls)) params$y <- cls
  
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