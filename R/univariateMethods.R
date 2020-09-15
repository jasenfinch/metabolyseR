#' ANOVA
#' @rdname anova
#' @description One-way analysis of variance (ANOVA).
#' @param x S4 object of class AnalysisData
#' @param cls vector of sample info column names to analyse
#' @param pAdjust p value adjustment method
#' @param comparisons list of comparisons to perform
#' @param returnModels should models be returned
#' @param nCores number of cores to use for parallelisation
#' @param clusterType cluster type to use for parallelisation (`?parallel::makeCluster`)
#' @importFrom dplyr bind_rows
#' @importFrom broom tidy
#' @export

setMethod('anova',signature = 'AnalysisData',
          function(x,cls = 'class', pAdjust = 'bonferroni', comparisons = list(), returnModels = F, nCores = detectCores() * 0.75, clusterType = getClusterType()){
            
            d <- x %>%
              dat()
            
            i <- x %>%
              sinfo() %>%
              select(all_of(cls))
            
            clsFreq <- i %>%
              group_by_all() %>%
              summarise(n = n(),.groups = 'drop')
            
            if (T %in% (clsFreq$n < 3)) {
              clsRem <- clsFreq %>%
                filter(n < 3)
              
              x <- x %>%
                removeClasses(cls = cls,classes = clsRem$class)
              
              warning(str_c('Classes with < 3 replicates removed: ',str_c(str_c('"',clsRem$class,'"'),collapse = ', ')),call. = F)
              
              i <- x %>%
                sinfo() %>%
                select(cls)
            }
            
            if (length(comparisons) > 0) {
              comp <- comparisons
            } else {
              comp <- map(i,~{unique(.) %>% sort() %>% str_c(collapse = '~')})
            }
            
            models <- comp %>%
              names() %>%
              map(~{
                pred <- .
                ps <- comp[[pred]]
                
                if (length(ps) < nCores) {
                  nCores <- length(ps)
                }
                
                clus <- makeCluster(nCores,type = clusterType)
                
                r <- ps %>%
                  parLapply(cl = clus,X = .,fun = function(z,da,pred){
                    p <- z  
                    pc <- str_split(p,'~')[[1]]
                    
                    pad <- removeClasses(da,pred,classes = sinfo(da) %>%
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
                  },da = x,pred = pred) %>%
                  set_names(comp[[pred]])
                stopCluster(clus)
                return(r)
              }) %>%
              set_names(names(comp))
            
            results <- models %>%
              map(~{
                map(.,~{
                  map(.,tidy) %>%
                    bind_rows(.id = 'Feature') %>%
                    mutate(adjusted.p.value = p.adjust(p.value,method = pAdjust))
                }) %>%
                  bind_rows(.id = 'Comparison')
              }) %>%
              bind_rows(.id = 'Response') %>%
              filter(term == 'response')
            
            res <- new('Univariate')
            res@type <- 'ANOVA'
            res@data <- x
            res@results <- results
            
            if (returnModels == T) {
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
#' @param nCores number of cores to use for parallelisation
#' @param clusterType cluster type to use for parallelisation (`?parallel::makeCluster`)
#' @importFrom dplyr bind_rows
#' @importFrom broom glance
#' @export

setMethod('ttest',signature = 'AnalysisData',
          function(x,cls = 'class', pAdjust = 'bonferroni', comparisons = list(), returnModels = F, nCores = detectCores() * 0.75, clusterType = getClusterType()){
            
            d <- x %>%
              dat()
            
            i <- x %>%
              sinfo()
            
            classes <- i %>%
              select(cls)
            
            if (length(comparisons > 0)) {
              pw <- comparisons
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
                
                r <- ps %>%
                  parLapply(cl = clus,X = .,fun = function(z,da,pred){
                    p <- z  
                    pc <- str_split(p,'~')[[1]]
                    
                    pad <- removeClasses(da,pred,classes = sinfo(da) %>%
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
                  },da = x,pred = pred) %>%
                  set_names(pw[[pred]])
                stopCluster(clus)
                return(r)
              }) %>%
              set_names(names(pw))
            
            results <- models %>%
              map(~{
                map(.,~{
                  map(.,glance) %>%
                    bind_rows(.id = 'Feature') %>%
                    mutate(adjusted.p.value = p.adjust(p.value,method = pAdjust))
                }) %>%
                  bind_rows(.id = 'Comparison')
              }) %>%
              bind_rows(.id = 'Response')
            
            res <- new('Univariate')
            res@type <- 't-test'
            res@data <- x
            res@results <- results
            
            if (returnModels == T) {
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
          function(x, cls = 'class', pAdjust = 'bonferroni', returnModels = F){
            indep <- x %>%
              sinfo() %>%
              select(cls)
            
            if (FALSE %in% (map_chr(indep,class) %in% c('integer','numeric'))) {
              stop('Independent variables need to be numeric')
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
                  mutate(adjusted.p.value = p.adjust(p.value,method = pAdjust))
              }) %>%
              bind_rows(.id = 'Response')
            
            res <- new('Univariate')
            res@type <- 'linear regression'
            res@data <- x
            res@results <- results
            
            if (returnModels == T) {
              res@models <- models
            } 
            
            return(res)
          }
)
            