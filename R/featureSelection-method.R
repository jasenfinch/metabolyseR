#' @importFrom parallel parLapply makeCluster stopCluster clusterEvalQ
#' @importFrom utils combn

setMethod("featureSelection", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == T) {
              startTime <- proc.time()
              cat(blue('Feature selection'),cli::symbol$continue,'\r',sep = '') 
            }
            
            parameters <- x@parameters@featureSelection
            methods <- parameters$method
            
            if (length(x@preTreated) > 0) {
              dat <- x %>%
                preTreatedData()
              cls <- x  %>%
                preTreatedInfo() %>%
                select(parameters$cls) %>%
                unlist() %>% sort()
            } else {
              dat <- x %>%
                rawData()
              cls <-  x  %>%
                rawInfo() %>%
                select(parameters$cls) %>%
                unlist() %>% sort()
            }
            
            res <- map(1:length(methods),~{
              type <- fsMethods(methods[.],description = T)$type
              
              method <- methods[.]
              
              if (type == 'discrimination') {
                com <- combn(unique(as.character(cls)),2)
                dat.pair <- apply(com,2,function(y,cls,dat){
                  dat <- dat[cls %in% y,]
                  dat <- bind_cols(cls = cls[cls %in% y],dat)
                  return(dat)
                },cls = cls,dat = dat)
                com <- apply(com,2,paste,collapse = '~')
                names(dat.pair) <- com
                
                if (length(parameters$pairwises) > 0) {
                  dat.pair <- dat.pair[names(dat.pair) %in% parameters$pairwises]
                  com <- names(dat.pair)
                }
                
                if (length(dat.pair) < parameters$nCores) {
                  nCores <- length(dat.pair)
                } else {
                  nCores <- parameters$nCores
                }
                clust = makeCluster(nCores, type = parameters$clusterType)
                res <- parLapply(clust,dat.pair,function(dat,pars,method){
                    m <- fsMethods(method)
                    if (!is.null(pars)) {
                      newPars <- formals(m)
                      newPars[names(pars[[method]])] <- pars[[method]]
                      formals(m) <- newPars
                    }
                    res <- m(dat)
                    return(res)
                },pars = parameters$pars,method = method)
                stopCluster(clust)
                names(res) <- com
                
                res <- res %>%
                  bind_rows(.id = 'Pairwise')
              }
              
              if (type == 'regression') {
                m <- fsMethods(method)
                if (!is.null(pars)) {
                  newPars <- formals(m)
                  newPars[names(pars[[method]])] <- pars[[method]]
                  formals(m) <- newPars
                }
                dat <- bind_cols(cls = cls,dat %>% as_tibble())
                res <- m(dat) %>%
                  mutate(Pairwise = NA)
              }
              res <- res %>%
                mutate(Type = type,Method = method) %>%
                select(Type,Method,Pairwise,Feature:Pvalue)
              return(res)
            }) %>%
              bind_rows()
            
            x@featureSelection <- res
            x@log$featureSelection <- date()
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              cat(blue('Feature selection '),'\t',green(cli::symbol$tick),' ',elapsed,'\n',sep = '')
            }
            return(x)
          }
)