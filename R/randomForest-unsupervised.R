
collateUnsupervised <- function(models,result){
  models %>% 
    map_dfr(~.x[[result]],
            .id = 'rep')
}

unsupervised <- function(x,
                         rf = list(),
                         reps = 1,
                         returnModels = FALSE,
                         seed = 1234,
                         ...){
  
  models <- future_map(1:reps,~{
    performRF(
      dat(x),
      cls = NULL,
      rf = rf,
      type = 'unsupervised',
      returnModel = returnModels
    )
  },.options = furrr_options(seed = seed)) %>%
    set_names(1:reps)
  
  res <- new('RandomForest',
             x,
             type = 'unsupervised',
             importances = collate(models,'importance',type = 'unsupervised') %>% 
               group_by(feature,metric) %>% 
               summarise(value = mean(value)),
             proximities = collate(models,'proximities',type = 'unsupervised'))
  
  
  if (isTRUE(returnModels)) {
    res@models <- collateModels(models)
  }
  
  return(res)  
}