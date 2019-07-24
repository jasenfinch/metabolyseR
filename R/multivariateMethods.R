
setMethod('randomForest',signature = 'AnalysisData',
          function(x, cls, reps = 1, returnModels = F, seed = 1234, ...){
            
            d <- x %>%
              dat()
            
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
                  randomForest::randomForest(d,y = pred)
                }) %>%
                  set_names(1:reps)
                 return(mod) 
              }) %>%
              set_names(colnames(i))
            
            return(list(models = models))
          }
)