#' @importFrom parallel parLapply makeCluster stopCluster clusterEvalQ
#' @importFrom utils combn

setMethod("featureSelection", signature = "Analysis",
          function(x){
            parameters <- x@parameters@featureSelection
            if (length(x@preTreated) > 0) {
              dat <- x@preTreated$Data
              cls <- factor(unlist(x@preTreated$Info[,parameters$cls]))
            } else {
              dat <- x@rawData$Data
              cls <- factor(unlist(x@rawData$Info[,parameters$cls]))
            }
            
            com <- combn(unique(as.character(cls)),2)
            dat.pair <- apply(com,2,function(y,cls,dat){
              dat <- dat[cls %in% y,]
              dat <- bind_cols(cls = cls[cls %in% y],dat)
              return(dat)
            },cls = cls,dat = dat)
            com <- apply(com,2,paste,collapse = '~')
            names(dat.pair) <- com
            if (length(dat.pair) < parameters$nCores) {
              nCores <- length(dat.pair)
            } else {
              nCores <- parameters$nCores
            }
            clust = makeCluster(nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(y,method,pars){
              res.method <- lapply(method,function(z,dat,pars){
                m <- fsMethods(z)
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
            res.method <- lapply(parameters$method,function(y,res.pair){
              res.pair <- lapply(res.pair,function(z,method,com){
                z[[method]]
                },method = y,com = names(res.pair))
              f <- res.pair[[1]]$Feature
              res.pair <- lapply(res.pair,function(z){z$Score})
              res.pair <- as.data.frame(res.pair)
              colnames(res.pair) <- com
              res.pair <- bind_cols(Feature = f,res.pair)
              return(res.pair)
            },res.pair = res.pair)
            names(res.method) <- parameters$method
            
            feat <- bind_rows(res.method,.id = 'Method')
            feat <- gather(feat,'Pairwise','Score',-(Method:Feature))
            
            x@featureSelection <- feat
            x@log$featureSelection <- date()
            return(x)
          }
)