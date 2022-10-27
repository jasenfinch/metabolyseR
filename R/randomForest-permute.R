nPerm <- function(n,k){choose(n,k) * factorial(k)}

#' @importFrom stats runif

permute <- function(x,cls,rf,type){
  params <- formals(randomForest::randomForest)
  params <- c(params,rf)
  params$x <- x %>% dat()
  ind <- x %>%
    sinfo() %>%
    select(all_of(cls)) %>%
    unlist(use.names = FALSE) %>% 
    sample()
  params$y <- ind
  params$strata <- ind
  
  model <- do.call(randomForest::randomForest,params)
  
  performanceMetrics(model,
                     type = type)
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
  
  future_map_dfr(1:n,
             ~permute(x = x,
                      cls = cls,
                      rf = rf,
                      type = type),
             .id = 'permutation',
             .options = furrr_options(
               seed = runif(1) %>% 
                 {. * 100000} %>% 
                 round()
             )) %>% 
    group_by(.metric) %>% 
    summarise(mean = mean(.estimate),
              sd = sd(.estimate))
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
                          as_tibble(.name_repair = 'minimal'))
          }) %>%
            {
              suppressMessages(bind_rows(.,.id = 'Permutation'))
            } %>%
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
