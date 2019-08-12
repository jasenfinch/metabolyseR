nPerm <- function(n,k){choose(n,k) * factorial(k)}

permute <- function(x,cls,rf,n = 1000, nCores, clusterType){
  i <- x %>%
    sinfo() %>%
    select(cls) %>%
    unlist(use.names = F)
  
  if (nPerm(length(i),length(unique(i))) < n) {
    n <- nPerm(length(i),length(unique(i)))
  }
  
  if (n < nCores) {
    nCores <- n
  }
  
  clus <- makeCluster(nCores,type = clusterType)
  
  models <- parLapply(clus,1:n,function(y,d,index){
    params <- formals(randomForest::randomForest)
    params$x <- d %>% dat()
    params$y <- sample(index)
    params <- c(params,rf)
    do.call(randomForest::randomForest,params)
  },d = x,index = i) %>%
    set_names(1:n)
  stopCluster(clus)
  
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
                mutate(.metric = 'margin'))
  
  if (length(permutations) > 0) {
    meas <- meas %>%
      left_join(permutations$measures, by = c("Predictor","Comparison", ".metric")) %>%
      mutate(Pvalue = pnorm(.estimate,Mean,SD,lower.tail = F)) %>%
      select(-Mean,-SD)
  }
  
  return(meas)
}

#' @importFrom dplyr rowwise

classificationImportance <- function(importances,permutations){
  imps <- importances %>%
    group_by(Predictor,Comparison,Feature,Measure) %>%
    summarise(Value = mean(Value))
  
  if (length(permutations) > 0) {
    lowertail <- list(MeanDecreaseGini = F,SelectionFrequency = F,FalsePositiveRate = T)
    
    imps <- imps %>%
      left_join(permutations$importance, by = c("Predictor","Comparison", "Feature", "Measure")) %>%
      split(.$Measure) %>%
      map(~{
        i <- .
        tail <- lowertail[[i$Measure[1]]]
        i %>%
          rowwise() %>%
          mutate(Pvalue = pnorm(Value,Mean,SD,lower.tail = tail)) %>%
          tbl_df()
      }) %>%
      bind_rows() %>%
      group_by(Measure) %>%
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
            tibble(sample = 1:length(m$y),obs = m$y,pred = m$predicted,margin = margin(m)) %>%
              bind_cols(m$votes %>%
                          as_tibble())
          }) %>%
            bind_rows(.id = 'Permutation') %>%
            mutate(Permutation = as.numeric(Permutation))
        }) %>%
          bind_rows(.id = 'Comparison')
      }) %>%
      bind_rows(.id = 'Predictor') 
  })
  
  class_metrics <- metric_set(accuracy,kap)
  
  meas <- preds %>%
    split(.$Predictor) %>%
    map(~{
      d <- .
      d %>%
        split(.$Comparison) %>%
        map(~{
          p <- .
          p %>%
            mutate(obs = factor(obs),pred = factor(pred)) %>%
            group_by(Predictor,Comparison,Permutation) %>%
            class_metrics(obs,estimate = pred)
        }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    bind_rows(suppressMessages(preds %>%
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
                                         group_by(Predictor,Comparison,Permutation) %>%
                                         roc_auc(obs,estimate)
                                     }) %>%
                                     bind_rows()
                                 }) %>%
                                 bind_rows())) %>%
    bind_rows(preds %>%
                group_by(Predictor,Comparison,Permutation) %>%
                summarise(.estimate = mean(margin)) %>%
                mutate(.metric = 'margin')) %>%
    group_by(Predictor,Comparison,.metric) %>%
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
    bind_rows(.id = 'Predictor') %>%
    gather('Measure','Value',-(Predictor:Feature)) %>%
    group_by(Predictor,Comparison,Feature,Measure) %>%
    summarise(Mean = mean(Value),SD = sd(Value))
  
  return(list(measures = meas,importance = imps))
}

regressionMeasures <- function(predictions,permutations){
  reg_metrics <- metric_set(rsq,mae,mape,rmse,ccc)
  meas <- predictions %>%
    split(.$Predictor) %>%
    map(~{
      d <- .
      d %>%
        group_by(Predictor) %>%
        reg_metrics(obs,estimate = pred)
    }) %>%
    bind_rows()
  
  if (length(permutations) > 0) {
    lowertail <- list(rsq = F,mae = T,mape = T,mape = T,rmse = T,ccc = F)
    
    meas <- meas %>%
      left_join(permutations$measures, by = c("Predictor", ".metric")) %>%
      rowwise() %>%
      mutate(Pvalue = pnorm(.estimate,Mean,SD,lower.tail = lowertail[[.metric]])) %>%
      select(-Mean,-SD)
  }
  
  return(meas)
}

regressionImportance <- function(importances,permutations){
  imps <- importances %>%
    group_by(Predictor,Feature,Measure) %>%
    summarise(Value = mean(Value)) 
  
  if (length(permutations) > 0) {
    imps <- imps %>%
      left_join(permutations$importance, by = c("Predictor", "Feature", "Measure")) %>%
      mutate(Pvalue = pnorm(Value,Mean,SD,lower.tail = F)) %>%
      group_by(Measure) %>%
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
        tibble(sample = 1:length(m$y),obs = m$y,pred = m$predicted)
      }) %>%
        bind_rows(.id = 'Permutation') %>%
        mutate(Permutation = as.numeric(Permutation))
    }) %>%
    bind_rows(.id = 'Predictor')
  
  reg_metrics <- metric_set(rsq,mae,mape,rmse,ccc)
  
  meas <- preds %>%
    split(.$Predictor) %>%
    map(~{
      d <- .
      d %>%
        group_by(Predictor,Permutation) %>%
        reg_metrics(obs,estimate = pred)
    }) %>%
    bind_rows() %>%
    group_by(Predictor,.metric) %>%
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
    bind_rows(.id = 'Predictor') %>%
    gather('Measure','Value',-Predictor,-Permutation,-Feature) %>%
    group_by(Predictor,Feature,Measure) %>%
    summarise(Mean = mean(Value),SD = sd(Value))
  
  return(list(measures = meas,importance = imps))
}

#' @importFrom forestControl fpr_fs

unsupervised <- function(x,rf,reps,returnModels,seed,nCores,clusterType,...){
  
  if (reps < nCores) {
    nCores <- reps
  }
  
  set.seed(seed)
  
  clus <- makeCluster(nCores,clusterType)
  
  models <- parLapply(clus,1:reps,function(y,d,rf){
    params <- formals(randomForest::randomForest)
    params$x <- d %>% dat()
    params <- c(params,rf)
    do.call(randomForest::randomForest,params)
  },d = x,rf = rf) %>%
    set_names(1:reps)
  
  stopCluster(clus)
  
  importances <- models %>%
    map(~{
      m <- .
      importance(m) %>%
        left_join(fpr_fs(m),by = c('Feature' = 'variable')) %>%
        rename(SelectionFrequency = freq,FalsePositiveRate = fpr)
    }) %>%
    bind_rows(.id = 'Rep') %>%
    mutate(Rep = as.numeric(Rep)) %>%
    gather('Measure','Value',-Rep,-Feature)
  
  proximities <- models %>%
    map(.,~{.$proximity %>%
        as_tibble() %>%
        mutate(Sample = 1:nrow(.)) %>%
        gather('Sample2','Proximity',-Sample) %>%
        rename(Sample1 = Sample)
    }) %>%
    bind_rows(.id = 'Rep') %>%
    mutate(Rep = as.numeric(Rep))  %>%
    mutate(Sample2 = as.numeric(Sample2))
  
  results <- list(
    importances = importances %>%
      select(-Rep) %>%
      group_by(Feature,Measure) %>%
      summarise(Value = mean(Value))
  )
  
  res <- new('RandomForest')
  res@type <- 'unsupervised'
  res@data <- x
  res@results <- results
  res@importances <- importances
  res@proximities <- proximities
  
  if (isTRUE(returnModels)) {
    res@models <- models
  }
  
  return(list(res))  
}

supervised <- function(x,cls,rf,reps,binary,comparisons,perm,returnModels,seed,nCores,clusterType){
  i <- x %>%
    sinfo() %>%
    select(cls)
  
  i %>%
    colnames() %>%
    map(~{
      cls <- .
      
      pred <- i %>%
        select(cls) %>%
        unlist()
      
      if (is.numeric(pred)) {
        regression(x,cls,rf,reps,perm,returnModels,seed,nCores,clusterType)
      } else {
        classification(x,cls,rf,reps,binary,comparisons,perm,returnModels,seed,nCores,clusterType)
      }
    }) %>%
    set_names(colnames(i))
}

#' @importFrom yardstick metric_set accuracy kap roc_auc
#' @importFrom dplyr summarise_all
#' @importFrom stringr str_split
#' @importFrom magrittr set_names

classification <- function(x,cls,rf,reps,binary,comparisons,perm,returnModels,seed,nCores,clusterType){
  
  i <- x %>%
    sinfo() %>%
    select(cls)
  
  if (length(comparisons) > 0) {
    comp <- comparisons
  } else {
    if (binary == T) {
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
          
          cda <- removeClasses(x,inf,classes = sinfo(x) %>%
                                 select(inf) %>%
                                 unlist() %>%
                                 unique() %>%
                                 .[!(. %in% comparison)])
          
          pred <- cda %>%
            sinfo() %>%
            select(inf) %>%
            unlist(use.names = F) %>%
            factor()
          
          if (reps < nCores) {
            nSlaves <- length(reps)
          } else {
            nSlaves <- nCores
          }
          
          set.seed(seed)
          
          clus <- makeCluster(nSlaves,type = clusterType)
          
          mod <- parLapply(clus,1:reps,function(y,d,pred,rf){
            params <- formals(randomForest::randomForest)
            params$x <- d %>% dat()
            params$y <- pred
            params <- c(params,rf)
            do.call(randomForest::randomForest,params)
          },d = cda,pred = pred,rf = rf) %>%
            set_names(1:reps)
          stopCluster(clus)
          
          mod <- list(models = mod)
          
          if (perm > 0) {
            perms <- permute(x,cls,rf,n = perm,nCores,clusterType)
            mod <- c(mod,list(permutations = perms))
          }
          
          return(mod) 
        }) %>%
        set_names(comps)
    }) %>%
    set_names(colnames(i))
  
  suppressWarnings({
    predictions <- models %>%
      map(~{
        map(.,~{
          map(.$models,~{
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
    bind_rows(.id = 'Predictor') %>%
    gather('Measure','Value',-(Predictor:Feature))
  
  proximities <- models %>%
    map(~{
      map(.,~{
        map(.$models,~{.$proximity %>%
            as_tibble() %>%
            mutate(Sample = 1:nrow(.)) %>%
            gather('Sample2','Proximity',-Sample) %>%
            rename(Sample1 = Sample)
        }) %>%
          bind_rows(.id = 'Rep') %>%
          mutate(Rep = as.numeric(Rep))
      }) %>%
        bind_rows(.id = 'Comparison')
    }) %>%
    bind_rows(.id = 'Predictor')  %>%
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
  res@data <- x
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

regression <- function(x,cls,rf,reps,perm,returnModels,seed,nCores,clusterType){
  i <- x %>%
    sinfo() %>%
    select(cls)
  
  models <- i %>%
    colnames() %>%
    map(~{
      inf <- .
      
      pred <- i %>%
        select(inf) %>%
        unlist(use.names = F)
      
      if (reps < nCores) {
        nSlaves <- reps
      } else {
        nSlaves <- nCores
      }
      
      set.seed(seed)
      
      clus <- makeCluster(nSlaves,type = clusterType)
      
      mod <- parLapply(clus,1:reps,function(y,d,pred,rf){
        params <- formals(randomForest::randomForest)
        params$x <- d %>% dat()
        params$y <- pred
        params <- c(params,rf)
        do.call(randomForest::randomForest,params)
      },d = x,pred = pred,rf = rf) %>%
        set_names(1:reps)
      
      stopCluster(clus)
      
      mod <- list(models = mod)
      
      if (perm > 0) {
        perms <- permute(x,cls,rf,n = perm,nCores,clusterType)
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
        tibble(sample = 1:length(m$y),obs = m$y,pred = m$predicted)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Predictor')
  
  importances <- models %>%
    map(~{
      map(.$models,~{
        m <- .
        importance(m)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Predictor') %>%
    gather('Measure','Value',-Predictor,-Rep,-Feature)
  
  proximities <- models %>%
    map(~{
      map(.$models,~{.$proximity %>%
          as_tibble() %>%
          mutate(Sample = 1:nrow(.)) %>%
          gather('Sample2','Proximity',-Sample) %>%
          rename(Sample1 = Sample)
      }) %>%
        bind_rows(.id = 'Rep') %>%
        mutate(Rep = as.numeric(Rep))
    }) %>%
    bind_rows(.id = 'Predictor') %>%
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
  res@data <- x
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

#' randomForest
#' @rdname randomForest
#' @description Perform random forest on an AnalysisData object
#' @param x S4 object of class AnalysisData
#' @param cls vector of info columns to use for predictor information. Set to NULL for unsupervised.
#' @param rf named list of arguments to pass to randomForest::randomForest
#' @param reps number of repetitions to perform
#' @param binary TRUE/FALSE should binary comparisons be performed. Ignored for unsupervised and regression. Ignored if \code{comparisons} specified.
#' @param comparisons list of comparisons to perform. Ignored for unsupervised and regression. See details. 
#' @param perm number of permutations to perform. Ignored for unsupervised.
#' @param returnModels TRUE/FALSE should model objects be returned.
#' @param seed random number seed
#' @param nCores number of cores to use for parallisation.
#' @param clusterType cluster type for parallisation
#' @details Specified class comparisons should be given as a list named according to \code{cls}. Comparisons should be given as class names separated by '~' (eg. '1~2~H').
#' @examples 
#' library(metaboData)
#' library(magrittr)
#' 
#' data(abr1)
#' x <- analysisData(abr1$neg,abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#' rf <- randomForest(x,cls = 'day')
#' @export

setMethod('randomForest',signature = 'AnalysisData',
          function(x, cls = 'class', rf = list(), reps = 1, binary = F, comparisons = list(), perm = 0, returnModels = F, seed = 1234, nCores = detectCores() * 0.75, clusterType = getClusterType()){
            
            rf$keep.forest <- T
            rf$proximity <- T
            
            if (is.null(cls)) {
              res <- unsupervised(x,rf,reps,returnModels,seed,nCores,clusterType)
            } else {
              res <- supervised(x,cls,rf,reps,binary,comparisons,perm,returnModels,seed,nCores,clusterType)
            }
            
            return(res)
            
          }
)

#' measures
#' @rdname measures
#' @description return measures results from a RandomForest object
#' @param x S4 object of class RandomForest
#' @export

setMethod('measures',signature = 'RandomForest',
          function(x){
            x@results$measures
          }
)

#' importance
#' @rdname importance
#' @description return feature importance resutls from a RandomForest or Univariate classes
#' @param x S4 object of class RandomForest or Univariate
#' @export

setMethod('importance',signature = 'RandomForest',
          function(x){
            x@results$importances
          }
)

#' @rdname importance
#' @export

setMethod('importance',signature = 'Univariate',
          function(x){
            x@results
          }
)

