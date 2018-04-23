#' @importFrom randomForest randomForest
#' @importFrom stats oneway.test t.test kruskal.test
#' @importFrom forestControl fpr_fs
#' @importFrom purrr map_df

fsMethods <- function(method = NULL, description = F){
  methods <- list(
    
    fs.rf = function(dat,nreps = 100){
      res <- map(1:nreps,~{
        cls <- factor(dat$cls)
        dat <- dat[,-1]
        res <- randomForest::randomForest(dat,y = cls,importance = T, keep.forest = T,ntree = 1000) %>%
          fpr_fs()
        res <- tibble(Feature = rownames(res),SelectionFrequency = res$freq,FPR = res$fpr)
        return(res)
      }) %>% 
        bind_rows(.id = 'Rep') %>%
        group_by(Feature) %>%
        summarise(Score = mean(SelectionFrequency),
                  Pvalue = mean(FPR)
                  )
      return(res)
    },
    
    fs.anova = function(dat,pAdjust = 'bonferroni'){
      cls <- factor(dat$cls)
      dat <- dat[,-1]
      feat <- colnames(dat)
      res <- dat %>%
        map_df(~{
        r <- oneway.test(. ~ cls)
        return(tibble(Score = r$statistic,Pvalue = r$p.value))
      }) %>%
        bind_cols(Feature = feat,.) %>%
        mutate(Pvalue = p.adjust(Pvalue,method = pAdjust))
      return(res)
    },
    
    fs.ttest = function(dat,pAdjust = 'bonferroni'){
      cls <- factor(dat$cls)
      dat <- dat[,-1]
      feat <- colnames(dat)
      res <- dat %>%
        map_df(~{
          r <- t.test(. ~ cls)
          return(tibble(Score = r$statistic,Pvalue = r$p.value))
        }) %>%
        bind_cols(Feature = feat,.) %>%
        mutate(Pvalue = p.adjust(Pvalue,method = pAdjust))
      return(res)
    },
    
    fs.kruskal = function(dat,pAdjust = 'bonferroni'){
      cls <- factor(dat$cls)
      dat <- dat[,-1]
      feat <- colnames(dat)
      res <- dat %>%
        map_df(~{
          r <- kruskal.test(. ~ cls)
          return(tibble(Score = r$statistic,Pvalue = r$p.value))
        }) %>%
        bind_cols(Feature = feat,.) %>%
        mutate(Pvalue = p.adjust(Pvalue,method = pAdjust))
      return(res)
    }
  )
  
  descriptions = list(
    fs.rf = list(description = 'Random Forest using selection frequency based false positive rate for variable importance',
                 arguments = c(nreps = 'number of replications'), 
                 score = 'Selection Frequency',
                 Pvalue = 'false positive rate'),
    fs.anova = list(description = 'One-way ANOVA', 
                    arguments = c(pAdjust = 'method for multiple testing p value correction'),
                    score = 'F statistic',
                    Pvalue = 'Adjusted p value'),
    fs.ttest = list(description = 'Welch t-test', 
                    arguments = c(pAdjust = 'method for multiple testing p value correction'),
                    score = 't statistic',
                    Pvalue = 'Adjusted p value'),
    fs.kruskal = list(description = 'Kruskal-Wallis Rank Sum Test', 
                      arguments = c(pAdjust = 'method for multiple testing p value correction'),
                      score = 'Kruskal-Wallis chi-squared',
                      Pvalue = 'Adjusted p value')
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      method <- descriptions[[method]]
    }
  }
  return(method)
}
