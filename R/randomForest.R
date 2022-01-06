nPerm <- function(n,k){choose(n,k) * factorial(k)}

#' @importFrom stats runif

permute <- function(x,cls,rf,n = 1000){
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
  
  models <- future_map(1:n,~{
    params <- formals(randomForest::randomForest)
    params <- c(params,rf)
    params$x <- x %>% dat()
    ind <- sample(i)
    params$y <- ind
    params$strata <- ind
    do.call(randomForest::randomForest,params)
  },.options = furrr_options(seed = runif(1) %>% 
    {. * 100000} %>% 
    round())) %>%
    set_names(1:n)
  
  return(models)
}

importance <- function(x){
  x %>%
    randomForest::importance() %>%
    {bind_cols(tibble(Feature = rownames(.)),as_tibble(.))}
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

classificationPermutationMeasures <- function(models){
  suppressWarnings({
    preds <- models %>%
      map(~{
        map(.,~{
          map(.$permutations,~{
            m <- .
            tibble(sample = seq_along(m$y),
                   obs = m$y,
                   pred = m$predicted,
                   margin = margin(m)) %>%
              bind_cols(m$votes %>%
                          as_tibble())
          }) %>%
            bind_rows(.id = 'Permutation') %>%
            mutate(Permutation = as.numeric(Permutation))
        }) %>%
          bind_rows(.id = 'Comparison')
      }) %>%
      bind_rows(.id = 'Response') 
  })
  
  class_metrics <- metric_set(accuracy,kap)
  
  meas <- preds %>%
    base::split(.$Response) %>%
    map(~{
      d <- .
      d %>%
        base::split(.$Comparison) %>%
        map(~{
          p <- .
          p %>%
            mutate(obs = factor(obs),pred = factor(pred)) %>%
            group_by(Response,Comparison,Permutation) %>%
            class_metrics(obs,estimate = pred)
        }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    bind_rows(
      suppressMessages(
        preds %>%
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
                  group_by(Response,Comparison,Permutation) %>%
                  roc_auc(obs,estimate)
              }) %>%
              bind_rows()
          }) %>%
          bind_rows())) %>%
    bind_rows(preds %>%
                group_by(Response,Comparison,Permutation) %>%
                summarise(.estimate = mean(margin)) %>%
                mutate(.metric = 'margin')) %>%
    group_by(Response,Comparison,.metric) %>%
    summarise(Mean = mean(.estimate),SD = sd(.estimate))
  
  imps <- models %>%
    map(~{
      map(.,~{
        map(.$permutations,~{
          m <- .
          importance(m) %>%
            left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
            rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
        }) %>%
          bind_rows(.id = 'Permutation') %>%
          mutate(Permutation = as.numeric(Permutation))
      }) %>%
        bind_rows(.id = 'Comparison')
    }) %>%
    bind_rows(.id = 'Response') %>%
    gather('Metric','Value',-(Response:Feature)) %>%
    group_by(Response,Comparison,Feature,Metric) %>%
    summarise(Mean = mean(Value),SD = sd(Value))
  
  return(list(measures = meas,importance = imps))
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

regressionPermutationMeasures <- function(models){
  preds <- models %>%
    map(~{
      map(.$permutations,~{
        m <- .
        tibble(sample = seq_along(m$y),obs = m$y,pred = m$predicted)
      }) %>%
        bind_rows(.id = 'Permutation') %>%
        mutate(Permutation = as.numeric(Permutation))
    }) %>%
    bind_rows(.id = 'Response')
  
  reg_metrics <- metric_set(rsq,mae,mape,rmse,ccc)
  
  meas <- preds %>%
    base::split(.$Response) %>%
    map(~{
      d <- .
      d %>%
        group_by(Response,Permutation) %>%
        reg_metrics(obs,estimate = pred)
    }) %>%
    bind_rows() %>%
    group_by(Response,.metric) %>%
    summarise(Mean = mean(.estimate),SD = sd(.estimate))
  
  imps <- models %>%
    map(~{
      map(.$permutations,~{
        m <- .
        importance(m)
      }) %>%
        bind_rows(.id = 'Permutation') %>%
        mutate(Permutation = as.numeric(Permutation))
    }) %>%
    bind_rows(.id = 'Response') %>%
    gather('Metric','Value',-Response,-Permutation,-Feature) %>%
    group_by(Response,Feature,Metric) %>%
    summarise(Mean = mean(Value),SD = sd(Value))
  
  return(list(measures = meas,importance = imps))
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
    map(~{
      m <- .
      importance(m) %>%
        left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
        rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
    }) %>%
    bind_rows(.id = 'Rep') %>%
    mutate(Rep = as.numeric(Rep)) %>%
    gather('Metric','Value',-Rep,-Feature)
  
  proximities <- models %>%
    map(.,~{.$proximity %>%
        as_tibble() %>%
        mutate(Sample = seq_len(nrow(.))) %>%
        gather('Sample2','Proximity',-Sample) %>%
        rename(Sample1 = Sample)
    }) %>%
    bind_rows(.id = 'Rep') %>%
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

#' @importFrom yardstick metric_set accuracy kap roc_auc
#' @importFrom dplyr summarise_all group_by_all n
#' @importFrom stringr str_split
#' @importFrom magrittr set_names

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
          map(.x$models,~{
            m <- .x
            tibble(sample = seq_along(m$y),
                   obs = m$y,
                   pred = m$predicted,
                   margin = margin(m)) %>%
              bind_cols(m$votes %>%
                          as_tibble(.name_repair = 'minimal') %>%
                          mutate_all(as.numeric))
          }) %>%
            bind_rows(.id = 'Rep') %>%
            mutate(Rep = as.numeric(Rep))
        }) %>%
          bind_rows(.id = 'Comparison')
      }) %>%
      bind_rows(.id = 'Response')
  })
  
  importances <- models %>%
    map(~{
      map(.,~{
        map(.$models,~{
          m <- .
          importance(m) %>%
            left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
            rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
        }) %>%
          bind_rows(.id = 'Rep') %>%
          mutate(Rep = as.numeric(Rep))
      }) %>%
        bind_rows(.id = 'Comparison')
    }) %>%
    bind_rows(.id = 'Response') %>%
    gather('Metric','Value',-(Response:Feature))
  
  proximities <- models %>%
    map(~{
      map(.,~{
        map(.$models,~{.$proximity %>%
            as_tibble() %>%
            mutate(Sample = seq_len(nrow(.))) %>%
            gather('Sample2','Proximity',-Sample) %>%
            rename(Sample1 = Sample)
        }) %>%
          bind_rows(.id = 'Rep') %>%
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
      map(.$models,~{
        m <- .
        tibble(sample = seq_along(m$y),obs = m$y,pred = m$predicted)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Response')
  
  importances <- models %>%
    map(~{
      map(.$models,~{
        m <- .
        importance(m)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Response') %>%
    gather('Metric','Value',-Response,-Rep,-Feature)
  
  proximities <- models %>%
    map(~{
      map(.$models,~{.$proximity %>%
          as_tibble() %>%
          mutate(Sample = seq_len(nrow(.))) %>%
          gather('Sample2','Proximity',-Sample) %>%
          rename(Sample1 = Sample)
      }) %>%
        bind_rows(.id = 'Rep') %>%
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

#' Random forest analysis
#' @rdname randomForest
#' @description Perform random forest on an `AnalysisData` object
#' @param x S4 object of class `AnalysisData`
#' @param cls vector of sample information columns to use for response variable information. Set to NULL for unsupervised.
#' @param rf named list of arguments to pass to `randomForest::randomForest`
#' @param reps number of repetitions to perform
#' @param binary TRUE/FALSE should binary comparisons be performed. Ignored for unsupervised and regression. Ignored if `comparisons` specified.
#' @param comparisons list of comparisons to perform. 
#' Ignored for unsupervised and regression. See details. 
#' @param perm number of permutations to perform. Ignored for unsupervised.
#' @param returnModels TRUE/FALSE should model objects be returned.
#' @param seed random number seed
#' @return An S4 object of class `RandomForest`.
#' @details Specified class comparisons should be given as a list named 
#' according to \code{cls}. Comparisons should be given as class names 
#' separated by '~' (eg. '1~2~H').
#' @examples 
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' plotMDS(rf,cls = 'day')
#' @export

setGeneric("randomForest", 
           function(
             x, 
             cls = 'class',
             rf = list(), 
             reps = 1, 
             binary = FALSE, 
             comparisons = list(), 
             perm = 0, 
             returnModels = FALSE, 
             seed = 1234)
             standardGeneric("randomForest"))

#' @rdname randomForest

setMethod('randomForest',signature = 'AnalysisData',
          function(x, 
                   cls = 'class', 
                   rf = list(), 
                   reps = 1, 
                   binary = FALSE, 
                   comparisons = list(), 
                   perm = 0, 
                   returnModels = FALSE, 
                   seed = 1234){
            
            rf$keep.forest <- TRUE
            rf$proximity <- TRUE
            rf$importance <- TRUE
            
            if (is.null(cls)) {
              res <- unsupervised(x,
                                  rf,
                                  reps,
                                  returnModels,
                                  seed)
            } else {
              res <- supervised(x,
                                cls,
                                rf,
                                reps,
                                binary,
                                comparisons,
                                perm,
                                returnModels,
                                seed)
            }
            
            if (is.null(cls) | length(cls) == 1) {
              res <- res[[1]]
            }
            
            return(res)
            
          }
)
