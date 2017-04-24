#' @rdname featureSelection
#' @importFrom parallel parLapply makeCluster stopCluster clusterEvalQ

setMethod("featureSelection", signature = "Analysis",
          function(object){
            parameters <- object@parameters$featureSelection
            cls <- factor(object@preTreated$Info[,parameters$cls])
            
            com <- combn(unique(as.character(cls)),2)
            dat.pair <- apply(com,2,function(x,cls,dat){
              dat <- dat[cls %in% x,]
              dat <- data.frame(cls = cls[cls %in% x],dat)
              return(dat)
            },cls = cls,dat = object@preTreated$Data)
            com <- apply(com,2,paste,collapse = '~')
            names(dat.pair) <- com
            clust = makeCluster(parameters$nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(x,method,pars){
              res.method <- lapply(method,function(y,dat,pars){
                m <- fsMethods(y)
                if (!is.null(pars)) {
                  newPars <- formals(method)
                  newPars[names(pars[[y]])] <- pars[[y]]
                  formals(m) <- newPars
                }
                res <- m(dat)
                return(res)
              },dat = x,pars = pars)
              names(res.method) <- method
              return(res.method)
            },method = parameters$method,pars = parameters$pars)
            stopCluster(clust)
            names(res.pair) <- com
            res.method <- lapply(parameters$method,function(x,res.pair){
              res.pair <- lapply(res.pair,function(y,method,com){
                y[[method]]
                },method = x,com = names(res.pair))
              f <- res.pair[[1]]$Feature
              res.pair <- lapply(res.pair,function(z){z$Score})
              res.pair <- as.data.frame(res.pair)
              colnames(res.pair) <- com
              rownames(res.pair) <- f
              return(res.pair)
            },res.pair = res.pair)
            names(res.method) <- parameters$method
            object@featureSelection <- res.method
            object@log$featureSelection <- date()
            return(object)
          }
)