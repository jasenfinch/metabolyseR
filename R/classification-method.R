#' @rdname classification
#' @importFrom FIEmspro valipars accest dat.sel1
#' @importFrom stringr str_count 
#' @importFrom parallel makeCluster parLapply clusterExport stopCluster

setMethod("classification", signature = "Analysis",
          function(x){
            parameters <- x@parameters@classification
            cls <- factor(x@preTreated$Info[,parameters$cls])
            if (is.null(parameters$pars)) {
              par <- valipars(parameters$pars$sampling,parameters$pars$niter,parameters$pars$nreps,parameters$pars$strat,parameters$pars$div)
            } else {
              newValipars <- valipars
              pars <- formals(newValipars)
              pars[names(parameters$pars)] <- parameters$pars
              formals(newValipars) <- pars
              par <- newValipars()
            }
            dat.pair <- dat.sel1(x@preTreated$Data,cls,pwise = unique(as.character(cls)),pars = par)
            com <- sapply(dat.pair, function(y){y$name})
            if (length(com) > 1) {
              dat.pair <- dat.pair[-which(sapply(com,str_count,pattern = '~') > 1)]
            }
            clust = makeCluster(parameters$nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(y,method,pars){
              res.meth <- lapply(method,function(z,dat,pars){
                accest(dat,clmeth = z,pars = pars)
              },dat = y,pars = pars)
              names(res.meth) <- method
              return(res.meth)
            },method = parameters$method,pars = parameters$pars)
            stopCluster(clust)
            names(res.pair) <- sapply(dat.pair, function(y){y$name})
            
            x@classification <- res.pair
            x@log$classification <- date()
            return(x)
          }
)
