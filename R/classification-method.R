#' @rdname classification
#' @importFrom FIEmspro valipars accest dat.sel1
#' @importFrom stringr str_count 
#' @importFrom parallel makeCluster parLapply clusterExport stopCluster

setMethod("classification", signature = "Analysis",
          function(object){
            parameters <- object@parameters$classification
            cls <- factor(object@preTreated$Info[,parameters$cls])
            
            par <- valipars(parameters$pars$sampling,parameters$pars$niter,parameters$pars$nreps,parameters$pars$strat,parameters$pars$div)
            
            dat.pair <- dat.sel1(object@preTreated$Data,cls,pwise = unique(as.character(cls)),pars = par)
            com <- sapply(dat.pair, function(x){x$name})
            if (length(com) > 1) {
              dat.pair <- dat.pair[-which(sapply(com,str_count,pattern = '~') > 1)]
            }
            clust = makeCluster(parameters$nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(x,method,pars){
              res.meth <- lapply(method,function(y,dat,pars){
                accest(dat,clmeth = y,pars = pars)
              },dat = x,pars = pars)
              names(res.meth) <- method
              return(res.meth)
            },method = parameters$method,pars = parameters$pars)
            stopCluster(clust)
            names(res.pair) <- sapply(dat.pair, function(x){x$name})
            
            object@classification <- res.pair
            object@log$classification <- date()
            return(object)
          }
)
