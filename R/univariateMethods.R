#' ttest
#' @rdname ttest
#' @importFrom dplyr mutate_all
#' @export

setMethod('ttest',signature = 'AnalysisData',
          function(x,cls,pAdjust = 'bonferroni'){
            
            d <- x %>%
              dat()
            
            i <- x %>%
              info()
            
            classes <- i %>%
              select(cls)
            
            pw <- classes %>%
              map(pairwises)
            
            res <- pw %>%
              map(~{
                cl <- .
                cl %>%
                  map(~{
                    p <- .  
                    
                  })
              })
            
            
            d %>%
              map_df(~{
                r <- t.test(. ~ cls)
                return(tibble(Score = r$statistic,Pvalue = r$p.value))
              }) %>%
              set_names(colnames(d)) %>%
              mutate(adjustedPvalue = p.adjust(Pvalue,method = pAdjust))
            return(res)
          }
)