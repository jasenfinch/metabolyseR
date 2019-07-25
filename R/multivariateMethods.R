importance <- function(x){
  x %>%
    randomForest::importance() %>%
    {bind_cols(tibble(Feature = rownames(.)),as_tibble(.))}
}

#' @importFrom forestControl fpr_fs

unsupervised <- function(x,reps,returnModels,seed,...){
  set.seed(seed)
  models <- map(1:reps,~{
    randomForest::randomForest(x %>% dat(),keep.forest = T)
  }) %>%
    set_names(1:reps)
  
  importances <- models %>%
    map(~{
      m <- .
      importance(m) %>%
        left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
        rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
    }) %>%
    bind_rows(.id = 'Rep') %>%
    mutate(Rep = as.numeric(Rep))
  
  proximities <- models %>%
    map(.,~{.$proximity %>%
        as_tibble() %>%
        mutate(Sample = nrow(.)) %>%
        gather('Sample2','Proximity',-Sample) %>%
        rename(Sample1 = Sample)
    }) %>%
    bind_rows(.id = 'Rep') %>%
    mutate(Rep = as.numeric(Rep))
  
  res <- list(type = 'unsupervised',importances = importances,proximities = proximities)
  
  if (isTRUE(returnModels)) {
    res <- c(res,list(models = models))  
  }
  
  return(res)  
}

supervised <- function(x,cls,reps,pairwise,comparisons,returnModels,seed){
  i <- x %>%
    info() %>%
    select(cls)
  
  i %>%
    colnames() %>%
    map(~{
      cls <- .
      
      pred <- i %>%
        select(cls) %>%
        unlist()
      
      if (is.numeric(pred)) {
        regression(x,cls,reps,returnModels,seed)
      } else {
        classification(x,cls,reps,pairwise,comparisons,returnModels,seed)
      }
    }) %>%
    set_names(colnames(i))
}

#' @importFrom yardstick metric_set accuracy kap roc_auc
#' @importFrom dplyr summarise_all

classification <- function(x,cls,reps,pairwise,comparisons,returnModels,seed){
  
  i <- x %>%
    info() %>%
    select(cls)
  
  if (length(comparisons) > 0) {
    comp <- comparisons
  } else {
    if (pairwise == T) {
      comp <- map(i,getPairwises) 
    } else {
      comp <- map(i,~{unique(.) %>% sort() %>% str_c(collapse = '~')})
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
          
          cda <- removeClasses(x,inf,classes = info(x) %>%
                                 select(inf) %>%
                                 unlist() %>%
                                 unique() %>%
                                 .[!(. %in% comparison)])
          
          pred <- cda %>%
            info() %>%
            select(inf) %>%
            unlist() %>%
            factor()
          
          set.seed(seed)
          mod <- map(1:reps,~{
            randomForest::randomForest(cda %>% dat(),y = pred,proximity = T)
          }) %>%
            set_names(1:reps)
          return(mod) 
        }) %>%
        set_names(comps)
    }) %>%
    set_names(colnames(i))
  
  suppressWarnings({
    predictions <- models %>%
      map(~{
        map(.,~{
          map(.,~{
            m <- .
            tibble(sample = 1:length(m$y),obs = m$y,pred = m$predicted,margin = margin(m)) %>%
              bind_cols(m$votes %>%
                          as_tibble())
          }) %>%
            bind_rows(.id = 'Rep') %>%
            mutate(Rep = as.numeric(Rep))
        }) %>%
          bind_rows(.id = 'Comparison')
      }) %>%
      bind_rows(.id = 'Predictor')
    
    importances <- models %>%
      map(~{
        map(.,~{
          map(.,~{
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
      bind_rows(.id = 'Predictor')
    
    proximities <- models %>%
      map(~{
        map(.,~{
          map(.,~{.$proximity %>%
              as_tibble() %>%
              mutate(Sample = nrow(.)) %>%
              gather('Sample2','Proximity',-Sample) %>%
              rename(Sample1 = Sample)
          }) %>%
            bind_rows(.id = 'Rep') %>%
            mutate(Rep = as.numeric(Rep))
        }) %>%
          bind_rows(.id = 'Comparison')
      }) %>%
      bind_rows(.id = 'Predictor')
    
    class_metrics <- metric_set(accuracy,kap)
    
    results <- list(
      measures = predictions %>%
        split(.$Predictor) %>%
        map(~{
          d <- .
          d %>%
            split(.$Comparison) %>%
            map(~{
              p <- .
              p %>%
                mutate(obs = factor(obs),pred = factor(pred)) %>%
                group_by(Predictor,Comparison) %>%
                class_metrics(obs,estimate = pred)
            }) %>%
            bind_rows()
        }) %>%
        bind_rows() %>%
        bind_rows(suppressMessages(predictions %>%
                                     split(.$Predictor) %>%
                                     map(~{
                                       d <- .
                                       d %>%
                                         split(.$Comparison) %>%
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
                                             group_by(Predictor,Comparison) %>%
                                             roc_auc(obs,estimate)
                                         }) %>%
                                         bind_rows()
                                     }) %>%
                                     bind_rows())) %>%
        bind_rows(predictions %>%
                    group_by(Predictor,Comparison) %>%
                    summarise(.estimate = mean(margin)) %>%
                    mutate(.metric = 'margin')),
      importances = importances %>%
        group_by(Predictor,Comparison,Feature) %>%
        summarise_all(mean)
    )
  })
  
  res <- list(type = 'classification',results = results, predictions = predictions,importances = importances,proximities = proximities)
  
  if (returnModels == T) {
    res <- c(res,list(models = models))
  } 
  return(res)
}

#' @importFrom yardstick rsq mae rmse ccc

regression <- function(x,cls,reps,returnModels,seed){
  i <- x %>%
    info() %>%
    select(cls)
  
  models <- i %>%
    colnames() %>%
    map(~{
      inf <- .
      
      pred <- i %>%
        select(inf) %>%
        unlist()
      
      set.seed(seed)
      mod <- map(1:reps,~{
        randomForest::randomForest(x %>% dat(),y = pred,proximity = T)
      }) %>%
        set_names(1:reps)
      return(mod) 
    }) %>%
    set_names(colnames(i))
  
  predictions <- models %>%
    map(~{
      map(.,~{
        m <- .
        tibble(sample = 1:length(m$y),obs = m$y,pred = m$predicted)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Predictor')
  
  importances <- models %>%
    map(~{
      map(.,~{
        m <- .
        importance(m)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Predictor')
  
  proximities <- models %>%
    map(~{
      map(.,~{.$proximity %>%
          as_tibble() %>%
          mutate(Sample = nrow(.)) %>%
          gather('Sample2','Proximity',-Sample) %>%
          rename(Sample1 = Sample)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Predictor')
  
  reg_metrics <- metric_set(rsq,mae,rmse,ccc)
  
  results <- list(
    measures = predictions %>%
      split(.$Predictor) %>%
      map(~{
        d <- .
        d %>%
          group_by(Predictor) %>%
          reg_metrics(obs,estimate = pred)
      }) %>%
      bind_rows(),
    importances = importances %>%
      group_by(Predictor,Feature) %>%
      summarise_all(mean)
  )
}

#' randomForest
#' @rdname randomForest
#' @export

setMethod('randomForest',signature = 'AnalysisData',
          function(x, cls = NULL, reps = 1, pairwise = F, comparisons = list(), returnModels = F, seed = 1234, ...){
            
            if (is.null(cls)) {
              res <- unsupervised(x,reps,returnModels,seed)
            } else {
              res <- supervised(x,cls,reps,pairwise,comparisons,returnModels,seed)
            }
            
            return(res)
            
          }
)

