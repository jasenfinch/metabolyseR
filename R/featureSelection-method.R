#' @importFrom parallel parLapply makeCluster stopCluster clusterEvalQ
#' @importFrom utils combn

setMethod("featureSelection", signature = "Analysis",
          function(x){
            parameters <- x@parameters@featureSelection
            if (length(x@preTreated) > 0) {
              dat <- x %>%
                preTreatedData()
              cls <- x  %>%
                preTreatedInfo() %>%
                select(parameters$cls) %>%
                unlist() %>%
                factor()
            } else {
              dat <- x %>%
                rawData()
              cls <-  x  %>%
                rawInfo() %>%
                select(parameters$cls) %>%
                unlist() %>%
                factor()
            }
            
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
            }
            
            if (length(dat.pair) < parameters$nCores) {
              nCores <- length(dat.pair)
            } else {
              nCores <- parameters$nCores
            }
            clust = makeCluster(nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(y,method,pars){
              res.method <- lapply(method,function(z,dat,pars){
                m <- metabolyseR:::fsMethods(z)
                if (!is.null(pars)) {
                  newPars <- formals(m)
                  newPars[names(pars[[z]])] <- pars[[z]]
                  formals(m) <- newPars
                }
                res <- m(dat)
                return(res)
              },dat = y,pars = pars)
              names(res.method) <- method
              return(res.method)
            },method = parameters$method,pars = parameters$pars)
            stopCluster(clust)
            names(res.pair) <- com
            
            feat <- res.pair %>%
              map(~{
              d <- .
              d %>%
                bind_rows(.id = 'Method')
            }) %>%
              bind_rows(.id = 'Pairwise')
            
            x@featureSelection <- feat
            x@log$featureSelection <- date()
            return(x)
          }
)