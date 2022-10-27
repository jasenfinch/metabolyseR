nPerm <- function(n,k){choose(n,k) * factorial(k)}

#' @importFrom stats runif

permute <- function(x,cls,rf,type){
  
  randomised_cls <- x %>%
    sinfo() %>%
    select(all_of(cls)) %>%
    unlist(use.names = FALSE) %>% 
    sample()
  
  rf$strata <- randomised_cls
  
  model <- performRF(dat(x),
                     randomised_cls,
                     rf,
                     type,
                     returnModel = FALSE)
  
  list(metrics = model$metrics,
       importance = model$importance)
}

permutations <- function(x,cls,rf,n,type){
  i <- x %>%
    sinfo() %>%
    select(cls) %>%
    unlist(use.names = FALSE)
  
  if (is.character(i) | is.factor(i)) {
    i <- factor(i)
  }
  
  if (nPerm(length(i),length(unique(i))) < n) {
    n <- nPerm(length(i),length(unique(i)))
  }
  
  permutation_results <- future_map(1:n,
                                    ~permute(x = x,
                                             cls = cls,
                                             rf = rf,
                                             type = type),
                                    .id = 'permutation',
                                    .options = furrr_options(
                                      seed = runif(1) %>% 
                                        {. * 100000} %>% 
                                        round()
                                    )) 
  
  permutation_metrics <- permutation_results %>%
    map_dfr(~.x$metrics,id = 'permutation') %>% 
    group_by(.metric) %>% 
    summarise(mean = mean(.estimate),
              sd = sd(.estimate))
  
  permutation_importance <- permutation_results %>%
    map_dfr(~.x$importance,id = 'permutation') %>% 
    group_by(feature,metric) %>% 
    summarise(mean = mean(value),
              sd = sd(value),
              .groups = 'drop')
  
  list(metrics = permutation_metrics,
       importance = permutation_importance)
}

collatePermutations <- function(models,type){
  switch(type,
         classification = collatePermutationsClassification(models),
         regression = collatePermutationsRegression(models))
}

collatePermutationsClassification <- function(models){
  permutation_metrics <- models %>% 
    map_dfr(
      ~.x %>% 
        map_dfr(~.x$permutations$metrics,
                .id = 'comparison'
        ),
      .id = 'response'
    )
  
  permutation_importance <- models %>% 
    map_dfr(
      ~.x %>% 
        map_dfr(~.x$permutations$importance,
                .id = 'comparison'
        ),
      .id = 'response'
    ) 
  
  list(metrics = permutation_metrics,
       importance = permutation_importance)  
}

collatePermutationsRegression <- function(models){
  permutation_metrics <- models %>% 
    map_dfr(
      ~.x$permutations$metrics,
      .id = 'response'
    )
  
  permutation_importance <- models %>% 
    map_dfr(
      ~.x$permutations$importance,
      .id = 'response'
    ) 
  
  list(metrics = permutation_metrics,
       importance = permutation_importance)
}

metricPvals <- function(x){
  model_type <- type(x)
  
  switch(model_type,
         classification = classificationMetricPvals(x),
         regression = regressionMetricPvals(x))
}

classificationMetricPvals <- function(x){
  left_join(x@metrics,
            x@permutations$metrics,
            by = c("response", "comparison", ".metric")) %>% 
    mutate(`p-value` = pnorm(.estimate,mean,sd,lower.tail = FALSE)) %>% 
    select(-mean,-sd)
}

regressionMetricPvals <- function(x){
  lowertail <- list(rsq = FALSE,
                    mae = TRUE,
                    mape = TRUE,
                    mape = TRUE,
                    rmse = TRUE,
                    ccc = FALSE)
  
  left_join(x@metrics,
            x@permutations$metrics,
            by = c("response", ".metric")) %>%
    rowwise() %>%
    mutate(`p-value` = pnorm(.estimate,
                          mean,
                          sd,
                          lower.tail = lowertail[[.metric]])) %>%
    select(-mean,-sd)
  
}

importancePvals <- function(x){
  model_type <- type(x)
  
  switch(model_type,
         classification = classificationImportancePvals(x),
         regression = regressionImportancePvals(x))
}

classificationImportancePvals <- function(x){
  lowertail <- list(MeanDecreaseGini = FALSE,
                    SelectionFrequency = FALSE,
                    FalsePositiveRate = TRUE)
  
  left_join(x@importances,
            x@permutations$importance, 
            by = c("response", "comparison", "feature",'metric')) %>% 
    rowwise() %>%
    mutate(`p-value` = pnorm(value,
                             mean,
                             sd,
                             lower.tail = lowertail[[metric]]),
           `adjusted_p-value` = p.adjust(`p-value`,
                                         method = 'bonferroni',
                                         n = nFeatures(x))) %>%
    select(-mean,-sd) %>% 
    ungroup()
}

regressionImportancePvals <- function(x){
  left_join(x@importances,
            x@permutations$importance, 
            by = c("response","feature",'metric')) %>% 
    rowwise() %>%
    mutate(`p-value` = pnorm(value,
                             mean,
                             sd,
                             lower.tail = FALSE),
           `adjusted_p-value` = p.adjust(`p-value`,
                                         method = 'bonferroni',
                                         n = nFeatures(x))) %>%
    select(-mean,-sd) %>% 
    ungroup()
}