#' @importFrom randomForest margin

classificationPredictions <- function(model){
  tibble(sample = seq_along(model$y),
         obs = model$y,
         pred = model$predicted,
         margin = margin(model)) %>%
    bind_cols(model$votes %>%
                as_tibble(.name_repair = 'minimal') %>%
                mutate_all(as.numeric))
}

classificationMetrics <- function(model){
  predictions <- model %>% 
    classificationPredictions()
  
  class_metrics <- metric_set(accuracy,kap)
  
  acc_kap <- predictions %>% 
    class_metrics(obs,estimate = pred)
  
  if (length(levels(predictions$obs)) > 2) {
    estimate <- levels(predictions$obs)
  } else {
    estimate <- levels(predictions$obs)[1]
  }
  
  roc <- predictions %>% 
    roc_auc(obs,estimate)
  
  bind_rows(
    acc_kap,
    roc,
    tibble(.metric = 'margin',
           .estimate = margin(model) %>% 
             mean())
  )
}

#' @importFrom forestControl fpr_fs

classificationImportance <- function(model){
  model %>% 
    randomForest::importance() %>%
    {bind_cols(tibble(feature = rownames(.)),as_tibble(.,.name_repair = 'minimal'))} %>% 
    left_join(fpr_fs(model),by = c('feature' = 'variable')) %>%
    rename(selection_frequency = freq,false_positive_rate = fpr) %>% 
    gather(metric,value,-feature)
}

collateClassification <- function(models,results){
  suppressMessages(
    models %>% 
      map_dfr(
        ~.x %>% 
          map_dfr(~.x$reps %>% 
                    map_dfr(~.x[[results]],
                            .id = 'rep'),
                  .id = 'comparison'
          ),
        .id = 'response'
      ) 
  )
}

#' @importFrom yardstick metric_set accuracy kap roc_auc
#' @importFrom dplyr summarise_all group_by_all n
#' @importFrom stringr str_split
#' @importFrom magrittr set_names
#' @importFrom furrr future_map_dfr

classification <- function(x,
                           cls,
                           rf = list(
                             keep.forest = TRUE,
                             proximity = TRUE,
                             importance = TRUE
                           ),
                           reps = 1,
                           binary = FALSE,
                           comparisons = list(),
                           perm = 0,
                           returnModels = FALSE,
                           seed = 1234){
  
  i <- x %>%
    sinfo() %>%
    select(all_of(cls))
  
  clsFreq <- i %>%
    group_by_all() %>%
    summarise(n = n(),.groups = 'drop')
  
  if (any(clsFreq$n < 5)) {
    clsRem <- clsFreq %>%
      filter(n < 5)
    
    x <- x %>%
      removeClasses(cls = cls,classes = clsRem %>%
                      select(all_of(cls)) %>%
                      deframe())
    
    cls_list <- clsRem %>%
      select(all_of(cls)) %>%
      deframe() %>%
      str_c('"',.,'"') %>% 
      str_c(.,collapse = ', ')
    
    warning(str_c('Classes with < 5 replicates removed: ',
                  cls_list),
            call. = FALSE)
    
    i <- x %>%
      sinfo() %>%
      select(cls)
  }
  
  if (length(unique(deframe(i))) < 2) {
    stop('Need at least two classes to do classification.',call. = FALSE) 
  }
  
  if (length(comparisons) > 0) {
    comp <- comparisons
  } else {
    if (binary == TRUE) {
      comp <- map(names(i),~{
        binaryComparisons(x,cls = .x) 
      }) %>%
        set_names(names(i))
    } else {
      comp <- map(i,~{unique(.) %>% 
          sort() %>% 
          str_c(collapse = '~')})
    }
  }
  
  models <- i %>%
    colnames() %>%
    map(~{
      inf <- .x
      
      comps <- comp[[inf]] 
      
      comps %>%
        map(~{
          comparison <- str_split(.x,'~')[[1]]
          
          cda <- keepClasses(x,inf,classes = comparison)
          
          pred <- cda %>%
            sinfo() %>%
            select(all_of(inf)) %>%
            deframe() %>%
            factor()
          
          predFreq <- pred %>%
            tibble(cls = .) %>%
            group_by_all() %>%
            summarise(n = n(),.groups = 'drop')
          
          if (length(unique(predFreq$n)) > 1) {
            message(
              str_c('Unbalanced classes detected. Stratifying ',
                    'sample size to the smallest class size.'))
            
            ss <- pred %>%
              table() %>%
              min() %>%
              rep(length(unique(pred)))
            
            rf <- c(rf,list(strata = pred,sampsize = ss))
          }
          
          models <- future_map(
            1:reps,~{
              performRF(
                dat(cda),
                pred,
                rf,
                type = 'classification',
                returnModel = returnModels)
            },
            .options = furrr_options(seed = seed)) %>%
            set_names(1:reps)
          
          if (perm > 0) {
            permutation_results <- permutations(cda,
                                                inf,
                                                rf,
                                                perm,
                                                type = 'classification')
          } else {
            permutations_results <- list()
          }
          
          return(
            list(reps = models,
                 permutations = permutation_results)
            )
        }) %>%
        set_names(comps)
    }) %>%
    set_names(colnames(i))
  
  res <- new('RandomForest',
             x,
             type = 'classification',
             response = cls,
             metrics = collate(models,'metrics',type = 'classification') %>% 
               group_by(response,comparison,.metric,.estimator) %>% 
               summarise(.estimate = mean(.estimate),
                         .groups = 'drop'),
             predictions = collate(models,'predictions',type = 'classification'),
             importances = collate(models,'importance',type = 'classification') %>% 
               group_by(response,comparison,feature,metric) %>% 
               summarise(value = mean(value),
               .groups = 'drop'),
             proximities = collate(models,'proximities',type = 'classification'),
             permutations = collatePermutations(models,type = 'classification'))
  
  
  if (isTRUE(returnModels)) {
    res@models <- collateModels(models)
  }
  
  return(res)
}
