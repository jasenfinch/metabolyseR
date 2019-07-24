#' ttest
#' @rdname ttest
#' @export

setMethod('ttest',signature = 'AnalysisData',
          function(x,cls,pAdjust = 'bonferroni'){
  
  d <- x %>%
    dat()
  
  i <- x %>%
    info()
  
  
  cls <- i %>%
    select(cls) %>%
    factor()
  
  res <- d %>%
    map_df(~{
      r <- t.test(. ~ cls)
      return(tibble(Score = r$statistic,Pvalue = r$p.value))
    }) %>%
    set_names(colnames(d)) %>%
    mutate(adjustedPvalue = p.adjust(Pvalue,method = pAdjust))
  return(res)
})