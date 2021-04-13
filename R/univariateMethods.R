#' ANOVA
#' @rdname anova
#' @description One-way analysis of variance (ANOVA).
#' @param x S4 object of class AnalysisData
#' @param cls vector of sample info column names to analyse
#' @param pAdjust p value adjustment method
#' @param comparisons list of comparisons to perform
#' @param returnModels should models be returned
#' @importFrom dplyr bind_rows
#' @importFrom broom tidy
#' @export

setMethod('anova',signature = 'AnalysisData',
          function(x,
                   cls = 'class', 
                   pAdjust = 'bonferroni', 
                   comparisons = list(), 
                   returnModels = FALSE){
            
            d <- x %>%
              dat()
            
            i <- x %>%
              sinfo() %>%
              select(all_of(cls))
            
            clsFreq <- i %>%
              group_by_all() %>%
              summarise(n = n(),.groups = 'drop')
            
            if (TRUE %in% (clsFreq$n < 3)) {
              clsRem <- clsFreq %>%
                filter(n < 3)
              
              x <- x %>%
                removeClasses(cls = cls,classes = clsRem$class)
              
              warning(
                str_c('Classes with < 3 replicates removed: ',
                      str_c(str_c('"',
                                  clsRem$class,'"'),
                            collapse = ', ')),
                call. = FALSE)
              
              i <- x %>%
                sinfo() %>%
                select(all_of(cls))
            }
            
            if (length(comparisons) > 0) {
              comp <- comparisons
            } else {
              comp <- map(i,~{unique(.) %>% sort() %>% 
                  str_c(collapse = '~')})
            }
            
            models <- comp %>%
              names() %>%
              map(~{
                pred <- .
                ps <- comp[[pred]]
                
                r <- ps %>%
                  future_map(~{
                    pc <- str_split(.x,'~')[[1]]
                    
                    pad <- removeClasses(x,pred,classes = sinfo(x) %>%
                                           select(pred) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% pc)])
                    
                    response <- pad %>%
                      sinfo() %>%
                      select(pred) %>%
                      unlist() %>%
                      factor()
                    
                    pad %>%
                      dat() %>%
                      map(~{
                        aov(. ~ response)
                      })
                  }) %>%
                  set_names(comp[[pred]])
                return(r)
              }) %>%
              set_names(names(comp))
            
            results <- models %>%
              map(~{
                map(.,~{
                  map(.,tidy) %>%
                    bind_rows(.id = 'Feature') %>%
                    mutate(adjusted.p.value = p.adjust(p.value,
                                                       method = pAdjust))
                }) %>%
                  bind_rows(.id = 'Comparison')
              }) %>%
              bind_rows(.id = 'Response') %>%
              filter(term == 'response')
            
            res <- new('Univariate')
            res@type <- 'ANOVA'
            dat(res) <- dat(x)
            sinfo(res) <- sinfo(x)
            res@results <- results
            
            if (returnModels == TRUE) {
              res@models <- models
            } 
            
            return(res)
          }
)

#' ttest
#' @rdname ttest
#' @description Welch t-test
#' @param x S4 object of class AnalysisData
#' @param cls vector of sample info column names to analyse
#' @param pAdjust p value adjustment method
#' @param comparisons named list of binary comparisons to analyse
#' @param returnModels should models be returned
#' @importFrom dplyr bind_rows
#' @importFrom broom glance
#' @export

setMethod('ttest',signature = 'AnalysisData',
          function(x,
                   cls = 'class', 
                   pAdjust = 'bonferroni', 
                   comparisons = list(), 
                   returnModels = FALSE){
            
            if (length(comparisons) > 0) {
              pw <- comparisons
            } else {
              pw <- cls %>%
                map(binaryComparisons,x = x) %>%
                set_names(cls)
            }
            
            models <- pw %>%
              names() %>%
              map(~{
                pred <- .
                ps <- pw[[pred]]
                
                r <- ps %>%
                  future_map(~{
        
                    pc <- str_split(.x,'~')[[1]]
                    
                    pad <- removeClasses(x,pred,classes = sinfo(x) %>%
                                           select(pred) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% pc)])
                    
                    response <- pad %>%
                      sinfo() %>%
                      select(pred) %>%
                      unlist() %>%
                      factor()
                    
                    pad %>%
                      dat() %>%
                      map(~{
                        t.test(. ~ response)
                      })
                  }) %>%
                  set_names(pw[[pred]])
                return(r)
              }) %>%
              set_names(names(pw))
            
            results <- models %>%
              map(~{
                map(.,~{
                  map(.,glance) %>%
                    bind_rows(.id = 'Feature') %>%
                    mutate(adjusted.p.value = p.adjust(p.value,
                                                       method = pAdjust))
                }) %>%
                  bind_rows(.id = 'Comparison')
              }) %>%
              bind_rows(.id = 'Response')
            
            res <- new('Univariate')
            res@type <- 't-test'
            dat(res) <- dat(x)
            sinfo(res) <- sinfo(x)
            res@results <- results
            
            if (returnModels == TRUE) {
              res@models <- models
            } 
            
            return(res)
          }
)

#' linearRegression
#' @rdname linearRegression
#' @description Linear regression
#' @param x S4 object of class AnalysisData
#' @param cls vector of sample info column names to regress
#' @param pAdjust p value adjustment method
#' @param returnModels should models be returned
#' @export

setMethod('linearRegression',signature = 'AnalysisData',
          function(x, 
                   cls = 'class', 
                   pAdjust = 'bonferroni', 
                   returnModels = FALSE){
            indep <- x %>%
              sinfo() %>%
              select(cls)
            
            if (FALSE %in% 
                (map_chr(indep,class) %in% c('integer','numeric'))) {
              stop('Independent variables need to be numeric',
                   call. = FALSE)
            }
            
            d <- x %>%
              dat()
            
            models <- indep %>%
              colnames() %>%
              map(~{
                i <- .
                
                pred <- indep %>%
                  select(i) %>%
                  unlist()
                
                d %>%
                  map(~{
                    lm(. ~ pred)
                  })
              }) %>%
              set_names(colnames(indep))
            
            results <- models %>%
              map(~{
                map(.,~{
                  glance(.)
                }) %>%
                  bind_rows(.id = 'Feature') %>%
                  mutate(adjusted.p.value = p.adjust(p.value,
                                                     method = pAdjust))
              }) %>%
              bind_rows(.id = 'Response')
            
            res <- new('Univariate')
            res@type <- 'linear regression'
            dat(res) <- dat(x)
            sinfo(res) <- sinfo(x)
            res@results <- results
            
            if (returnModels == TRUE) {
              res@models <- models
            } 
            
            return(res)
          }
)
