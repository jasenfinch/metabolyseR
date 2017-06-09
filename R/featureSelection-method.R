#' @rdname featureSelection
#' @importFrom parallel parLapply makeCluster stopCluster clusterEvalQ

setMethod("featureSelection", signature = "Analysis",
          function(x){
            parameters <- x@parameters@featureSelection
            cls <- factor(unlist(x@preTreated$Info[,parameters$cls]))
            
            com <- combn(unique(as.character(cls)),2)
            dat.pair <- apply(com,2,function(y,cls,dat){
              dat <- dat[cls %in% y,]
              dat <- data.frame(cls = cls[cls %in% y],dat)
              return(dat)
            },cls = cls,dat = as.matrix(x@preTreated$Data))
            com <- apply(com,2,paste,collapse = '~')
            names(dat.pair) <- com
            clust = makeCluster(parameters$nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(y,method,pars){
              res.method <- lapply(method,function(z,dat,pars){
                m <- fsMethods(z)
                if (!is.null(pars)) {
                  newPars <- formals(method)
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
              rownames(res.pair) <- f
              return(res.pair)
            },res.pair = res.pair)
            names(res.method) <- parameters$method
            x@featureSelection <- res.method
            x@log$featureSelection <- date()
            return(x)
          }
)