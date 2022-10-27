regressionPredictions <- function(model){
  tibble(sample = seq_along(model$y),
         obs = model$y,
         pred = model$predicted)
}

regressionMetrics <- function(model){
  predictions <- model %>% 
    regressionPredictions()
  
  reg_metrics <- metric_set(rsq,mae,mape,rmse,ccc)
  
  predictions %>% 
    reg_metrics(obs,estimate = pred)
}

# regressionMeasures <- function(predictions,permutations){
  # reg_metrics <- metric_set(rsq,mae,mape,rmse,ccc)
#   meas <- predictions %>%
#     base::split(.$Response) %>%
#     map(~{
#       d <- .
#       d %>%
#         group_by(Response) %>%
#         reg_metrics(obs,estimate = pred)
#     }) %>%
#     bind_rows()
#   
#   if (length(permutations) > 0) {
#     lowertail <- list(rsq = FALSE,
#                       mae = TRUE,
#                       mape = TRUE,
#                       mape = TRUE,
#                       rmse = TRUE,
#                       ccc = FALSE)
#     
#     meas <- meas %>%
#       left_join(permutations$measures, by = c("Response", ".metric")) %>%
#       rowwise() %>%
#       mutate(Pvalue = pnorm(.estimate,
#                             Mean,
#                             SD,
#                             lower.tail = lowertail[[.metric]])) %>%
#       select(-Mean,-SD)
#   }
#   
#   return(meas)
# }

# regressionImportance <- function(importances,permutations){
#   imps <- importances %>%
#     group_by(Response,Feature,Metric) %>%
#     summarise(Value = mean(Value)) 
#   
#   if (length(permutations) > 0) {
#     imps <- imps %>%
#       left_join(permutations$importance,
#                 by = c("Response", "Feature", "Metric")) %>%
#       mutate(Pvalue = pnorm(Value,Mean,SD,lower.tail = FALSE)) %>%
#       group_by(Metric) %>%
#       mutate(adjustedPvalue = p.adjust(Pvalue,method = 'bonferroni')) %>%
#       select(-Mean,-SD)
#   }
#   
#   return(imps)
# }

collateRegression <- function(models,results){
  models %>% 
    map_dfr(
      ~.x$reps %>% 
        map_dfr(~.x[[results]],
                .id = 'rep'),
      .id = 'response'
    )
}
c
#' @importFrom yardstick rsq mae mape rmse ccc

regression <- function(x,
                       cls = 'class',
                       rf = list(
                         keep.forest = TRUE,
                         proximity = TRUE,
                         importance = TRUE
                       ),
                       reps = 1,
                       perm = 0,
                       returnModels = FALSE,
                       seed = 1234){
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
      
      models <- future_map(1:reps,~{
        performRF(dat(x),
                  pred,
                  rf,
                  type = 'regression',
                  returnModel = returnModels)
      },
      .options = furrr_options(seed = seed)) %>%
        set_names(1:reps)
      
      if (perm > 0) {
        permutation_results <- permutations(cda,
                                            inf,
                                            rf,
                                            perm,
                                            type = 'regression')
      } else {
        permutations_results <- list()
      }
      
      return(
        list(reps = models,
             permutations = permutation_results)
      ) 
    }) %>%
    set_names(colnames(i))
  
  res <- new('RandomForest',
             x,
             type = 'regression',
             response = cls,
             metrics = collate(models,'metrics',type = 'regression') %>% 
               group_by(response,.metric,.estimator) %>% 
               summarise(.estimate = mean(.estimate)),
             predictions = collate(models,'predictions',type = 'regression'),
             importances = collate(models,'importance',type = 'regression') %>% 
               group_by(response,feature,metric) %>% 
               summarise(value = mean(value)),
             proximities = collate(models,'proximities',type = 'regression'),
             permutations = collatePermutations(models,type = 'regression'))
  
  
  if (isTRUE(returnModels)) {
    res@models <- collateModels(models)
  }
  
  return(res)
}
