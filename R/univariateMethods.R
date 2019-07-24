#' ttest
#' @rdname ttest
#' @importFrom dplyr bind_rows
#' @export

setMethod('ttest',signature = 'AnalysisData',
          function(x,cls,pAdjust = 'bonferroni', pairwises = list(), returnModels = F, nCores = detectCores() * 0.75, clusterType = getClusterType(), ...){
            
            d <- x %>%
              dat()
            
            i <- x %>%
              info()
            
            classes <- i %>%
              select(cls)
            
            if (length(pairwises > 0)) {
              pw <- pairwises
            } else {
              pw <- classes %>%
                map(getPairwises) 
            }
            
            models <- pw %>%
              names() %>%
              map(~{
                pred <- .
                ps <- pw[[pred]]
                
                if (length(ps) < nCores) {
                  nCores <- length(ps)
                }
                
                clus <- makeCluster(nCores,type = clusterType)
                
                ps %>%
                  parLapply(cl = clus,X = .,fun = function(z,da,pred){
                    p <- z  
                    pc <- str_split(p,'~')[[1]]
                    
                    pad <- removeClasses(da,pred,classes = info(da) %>%
                                           select(pred) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% pc)])
                    
                    response <- pad %>%
                      info() %>%
                      select(pred) %>%
                      unlist() %>%
                      factor()
                    
                    pad %>%
                      dat() %>%
                      map(~{
                        t.test(. ~ response)
                      })
                  },da = x,pred = pred) %>%
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
           
            if (returnModels == T) {
              return(list(models = models,results = results))
            } else {
              return(list(results = results))
            }  
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