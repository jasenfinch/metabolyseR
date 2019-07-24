#' ttest
#' @rdname ttest
#' @importFrom dplyr mutate_all
#' @export

setMethod('ttest',signature = 'AnalysisData',
          function(x,cls,pAdjust = 'bonferroni', returnModels = F, ...){
            
            d <- x %>%
              dat()
            
            i <- x %>%
              info()
            
            classes <- i %>%
              select(cls)
            
            pw <- classes %>%
              map(pairwises)
            
            models <- pw %>%
              names() %>%
              map(~{
                cl <- .
                pw[[cl]] %>%
                  map(~{
                    p <- .  
                    pc <- str_split(p,'~')[[1]]
                    
                    pad <- removeClasses(x,cl,classes = info(x) %>%
                                           select(cl) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% pc)])
                    
                    response <- pad %>%
                      info() %>%
                      select(cl) %>%
                      unlist() %>%
                      factor()
                    
                    pad %>%
                      dat() %>%
                      map(~{
                        t.test(. ~ response)
                      })
                  }) %>%
                set_names(pw[[cl]])
              }) %>%
              set_names(names(pw))
            
            results <- models %>%
              map(~{
                map(.,~{
                  map(.,glance) %>%
                    bind_rows(.id = 'Feature') %>%
                    mutate(adjusted.p.value = p.adjust(p.value,method = pAdjust))
                }) %>%
                  bind_rows(.id = 'Pairwise')
              }) %>%
              bind_rows(.id = 'Predictor')
              
          }
)


#             d %>%
#               map_df(~{
#                 r <- t.test(. ~ cls)
#                 return(tibble(Score = r$statistic,Pvalue = r$p.value))
#               }) %>%
#               set_names(colnames(d)) %>%
#               mutate(adjustedPvalue = p.adjust(Pvalue,method = pAdjust))
#             return(res)
#           }
# )