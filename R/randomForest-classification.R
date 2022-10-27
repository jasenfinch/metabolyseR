
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

#' @importFrom randomForest margin
#' @importFrom stats pnorm

classificationMeasures <- function(predictions,permutations){
  
  class_metrics <- metric_set(accuracy,kap)
  
  meas <- predictions %>%
    base::split(.$Response) %>%
    map(~{
      d <- .
      d %>%
        base::split(.$Comparison) %>%
        map(~{
          p <- .
          p %>%
            mutate(obs = factor(obs),pred = factor(pred)) %>%
            group_by(Response,Comparison) %>%
            class_metrics(obs,estimate = pred)
        }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    bind_rows(
      suppressMessages(
        predictions %>%
          base::split(.$Response) %>%
          map(~{
            d <- .
            d %>%
              base::split(.$Comparison) %>%
              map(~{
                p <- .
                
                p <- p %>%
                  mutate(obs = factor(obs),pred = factor(pred)) 
                if (length(levels(p$obs)) > 2) {
                  estimate <- levels(p$obs)
                } else {
                  estimate <- levels(p$obs)[1]
                }
                p %>%
                  group_by(Response,Comparison) %>%
                  roc_auc(obs,estimate)
              }) %>%
              bind_rows()
          }) %>%
          bind_rows())) %>%
    bind_rows(predictions %>%
                group_by(Response,Comparison) %>%
                summarise(.estimate = mean(margin)) %>%
                mutate(.metric = 'margin'))
  
  if (length(permutations) > 0) {
    meas <- meas %>%
      left_join(
        permutations$measures, 
        by = c("Response","Comparison", ".metric")) %>%
      mutate(Pvalue = pnorm(.estimate,Mean,SD,lower.tail = FALSE)) %>%
      select(-Mean,-SD)
  }
  
  return(meas)
}

#' @importFrom dplyr rowwise

classificationImportance <- function(importances,permutations){
  imps <- importances %>%
    group_by(Response,Comparison,Feature,Metric) %>%
    summarise(Value = mean(Value))
  
  if (length(permutations) > 0) {
    lowertail <- list(MeanDecreaseGini = FALSE,
                      SelectionFrequency = FALSE,
                      FalsePositiveRate = TRUE)
    
    imps <- imps %>%
      left_join(
        permutations$importance,
        by = c("Response","Comparison", "Feature", "Metric")) %>%
      base::split(.$Metric) %>%
      map(~{
        i <- .
        tail <- lowertail[[i$Metric[1]]]
        i %>%
          rowwise() %>%
          mutate(Pvalue = pnorm(Value,Mean,SD,lower.tail = tail)) %>%
          ungroup()
      }) %>%
      bind_rows() %>%
      group_by(Metric) %>%
      mutate(adjustedPvalue = p.adjust(Pvalue,method = 'bonferroni')) %>%
      select(-Mean,-SD)
  }
  
  return(imps)
}

#' @importFrom yardstick metric_set accuracy kap roc_auc
#' @importFrom dplyr summarise_all group_by_all n
#' @importFrom stringr str_split
#' @importFrom magrittr set_names
#' @importFrom furrr future_map_dfr

classification <- function(x,
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
      inf <- .
      
      comps <- comp[[inf]] 
      
      comps %>%
        map(~{
          comparison <- str_split(.,'~')[[1]]
          
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
          
          set.seed(seed)
          
          mod <- future_map(1:reps,~{
            params <- formals(randomForest::randomForest)
            params$x <- cda %>% dat()
            params$y <- pred
            params <- c(params,rf)
            do.call(randomForest::randomForest,params)
          },.options = furrr_options(seed = seed)) %>%
            set_names(1:reps)
          
          mod <- list(models = mod)
          
          if (perm > 0) {
            perms <- permute(x,cls,rf,n = perm)
            mod <- c(mod,list(permutations = perms))
          }
          
          return(mod) 
        }) %>%
        set_names(comps)
    }) %>%
    set_names(colnames(i))
  
  suppressMessages({
    predictions <- models %>%
      map(~{
        map(.x,~{
          future_map_dfr(.x$models,
                         classificationPredictions,
          .id = 'Rep',
          .options = furrr_options(seed = seed)) %>%
            mutate(Rep = as.numeric(Rep))
        }) %>%
          bind_rows(.id = 'Comparison')
      }) %>%
      bind_rows(.id = 'Response')
  })
  
  importances <- models %>%
    map(~{
      map(.,~{
        future_map_dfr(.$models,~{
          m <- .
          importance(m) %>%
            left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
            rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
        },
        .id = 'Rep',
        .options = furrr_options(seed = seed)) %>%
          mutate(Rep = as.numeric(Rep))
      }) %>%
        bind_rows(.id = 'Comparison')
    }) %>%
    bind_rows(.id = 'Response') %>%
    gather('Metric','Value',-(Response:Feature))
  
  proximities <- models %>%
    map(~{
      map(.,~{
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
        bind_rows(.id = 'Comparison')
    }) %>%
    bind_rows(.id = 'Response')  %>%
    mutate(Sample2 = as.numeric(Sample2))
  
  if (perm > 0) {
    permutations <- classificationPermutationMeasures(models)
  } else {
    permutations <- list()
  }
  
  results <- list(
    measures = classificationMeasures(predictions,permutations),
    importances = classificationImportance(importances,permutations)
  )
  
  res <- new('RandomForest')
  res@type <- 'classification'
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
