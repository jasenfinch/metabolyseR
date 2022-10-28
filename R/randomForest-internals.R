
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
         unsupervised = tibble(),
         classification = classificationMetrics(model),
         regression = regressionMetrics(model))
}

modelPredictions <- function(model,type){
  switch(type,
         unsupervised = tibble(),
         classification = classificationPredictions(model),
         regression = regressionPredictions(model))
}

modelImportance <- function(model,type){
  switch(type,
         unsupervised = regressionImportance(model),
         classification = classificationImportance(model),
         regression = regressionImportance(model))
}

modelProximities <- function(model){
  model$proximity %>%
    as_tibble(.name_repair = 'minimal') %>%
    mutate(sample = seq_len(nrow(.))) %>%
    gather('sample2','proximity',-sample) %>%
    rename(sample1 = sample)
}

collate <- function(models,results,type){
  switch(type,
         unsupervised = collateUnsupervised(models,results),
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