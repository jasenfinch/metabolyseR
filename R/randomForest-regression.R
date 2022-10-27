regressionPredictions <- function(model){
  tibble(sample = seq_along(model$y),
         obs = model$y,
         pred = model$predicted)
}

regressionMeasures <- function(predictions,permutations){
  reg_metrics <- metric_set(rsq,mae,mape,rmse,ccc)
  meas <- predictions %>%
    base::split(.$Response) %>%
    map(~{
      d <- .
      d %>%
        group_by(Response) %>%
        reg_metrics(obs,estimate = pred)
    }) %>%
    bind_rows()
  
  if (length(permutations) > 0) {
    lowertail <- list(rsq = FALSE,
                      mae = TRUE,
                      mape = TRUE,
                      mape = TRUE,
                      rmse = TRUE,
                      ccc = FALSE)
    
    meas <- meas %>%
      left_join(permutations$measures, by = c("Response", ".metric")) %>%
      rowwise() %>%
      mutate(Pvalue = pnorm(.estimate,
                            Mean,
                            SD,
                            lower.tail = lowertail[[.metric]])) %>%
      select(-Mean,-SD)
  }
  
  return(meas)
}

regressionImportance <- function(importances,permutations){
  imps <- importances %>%
    group_by(Response,Feature,Metric) %>%
    summarise(Value = mean(Value)) 
  
  if (length(permutations) > 0) {
    imps <- imps %>%
      left_join(permutations$importance,
                by = c("Response", "Feature", "Metric")) %>%
      mutate(Pvalue = pnorm(Value,Mean,SD,lower.tail = FALSE)) %>%
      group_by(Metric) %>%
      mutate(adjustedPvalue = p.adjust(Pvalue,method = 'bonferroni')) %>%
      select(-Mean,-SD)
  }
  
  return(imps)
}

#' @importFrom yardstick rsq mae mape rmse ccc

regression <- function(x,
                       cls,
                       rf,
                       reps,
                       perm,
                       returnModels,
                       seed){
  i <- x %>%
    sinfo() %>%
    select(cls)
  
  models <- i %>%
    colnames() %>%
    map(~{
      inf <- .
      
      pred <- i %>%
        select(inf) %>%
        unlist(use.names = FALSE)
      
      set.seed(seed)
      
      mod <- future_map(1:reps,~{
        params <- formals(randomForest::randomForest)
        params$x <- x %>% dat()
        params$y <- pred
        params <- c(params,rf)
        do.call(randomForest::randomForest,params)
      },.options = furrr_options(seed = seed)) %>%
        set_names(1:reps)
      
      mod <- list(models = mod)
      
      if (perm > 0) {
        perms <- permute(x,cls,rf,n = perm)
      } else {
        perms <- list()
      }
      
      mod <- c(mod,list(permutations = perms))
      
      return(mod) 
    }) %>%
    set_names(colnames(i))
  
  predictions <- models %>%
    map(~{
      future_map_dfr(.$models,~{
        m <- .
        tibble(sample = seq_along(m$y),obs = m$y,pred = m$predicted)
      },
      .id = 'Rep',
      .options = furrr_options(seed = seed)) %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Response')
  
  importances <- models %>%
    map(~{
      future_map_dfr(.$models,~{
        m <- .
        importance(m)
      },
      .id = 'Rep',
      .options = furrr_options(seed = seed)) %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Response') %>%
    gather('Metric','Value',-Response,-Rep,-Feature)
  
  proximities <- models %>%
    map(~{
      future_map_dfr(.$models,~{.$proximity %>%
          as_tibble() %>%
          mutate(Sample = seq_len(nrow(.))) %>%
          gather('Sample2','Proximity',-Sample) %>%
          rename(Sample1 = Sample)
      },
      .id = 'Rep',
      .options = furrr_options(seed = seed)) %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Response') %>%
    mutate(Sample2 = as.numeric(Sample2))
  
  if (perm > 0) {
    permutations <- regressionPermutationMeasures(models) 
  } else {
    permutations <- list()
  }
  
  results <- list(
    measures = regressionMeasures(predictions,permutations),
    importances = regressionImportance(importances,permutations)
  )
  
  res <- new('RandomForest')
  res@type <- 'regression'
  res@response <- cls
  dat(res) <- dat(x)
  sinfo(res) <- sinfo(x)
  res@results <- results
  res@predictions <- predictions
  res@permutations <- permutations
  res@importances <- importances
  res@proximities <- proximities
  
  if (isTRUE(returnModels)) {
    res@models <- models
  }
  
  return(res)
}
