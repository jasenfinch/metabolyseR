#' @importFrom FIEmspro valipars accest dat.sel1
#' @importFrom stringr str_count 
#' @importFrom parallel makeCluster parLapply clusterExport stopCluster
#' @importFrom dplyr bind_rows

setMethod("classification", signature = "Analysis",
          function(x){
            parameters <- x@parameters@classification
            if (length(x@preTreated) > 0) {
              dat <- x@preTreated$Data
              cls <- factor(unlist(x@preTreated$Info[,parameters$cls]))
            } else {
              dat <- x@rawData$Data
              cls <- factor(unlist(x@rawData$Info[,parameters$cls]))
            }
            
            if (is.null(parameters$pars)) {
              par <- valipars(parameters$pars$sampling,parameters$pars$niter,parameters$pars$nreps,parameters$pars$strat,parameters$pars$div)
            } else {
              newValipars <- valipars
              pars <- formals(newValipars)
              pars[names(parameters$pars)] <- parameters$pars
              formals(newValipars) <- pars
              par <- newValipars()
            }
            dat.pair <- dat.sel1(dat,cls,pwise = sort(unique(as.character(cls))),pars = par)
            com <- sapply(dat.pair, function(y){y$name})
            if (length(com) > 1) {
              dat.pair <- dat.pair[-which(sapply(com,str_count,pattern = '~') > 1)]
            }
            if (length(dat.pair) < parameters$nCores) {
              nCores <- length(dat.pair)
            } else {
              nCores <- parameters$nCores
            }
            clust = makeCluster(nCores, type = parameters$clusterType)
            res.pair <- parLapply(clust,dat.pair,function(y,method,pars){
              res.meth <- lapply(method,function(z,dat,pars){
                accest(dat,clmeth = z,pars = pars)
              },dat = y,pars = pars)
              names(res.meth) <- method
              return(res.meth)
            },method = parameters$method,pars = parameters$pars)
            stopCluster(clust)
            names(res.pair) <- sapply(dat.pair, function(y){y$name})
            
            classi <- lapply(res.pair,function(y){
              y <- lapply(y,function(z){
                z <- bind_rows(list(Accuracy = z$acc.iter,AUC = z$auc.iter,Margin = z$mar.iter), .id = 'Iteration')
                z$Iteration <- 1:nrow(z)
                z <- gather(z,'Measure','Value',-Iteration)
                return(z)
              })
              y <- bind_rows(y,.id = 'Method')
              return(y)
            })
            
            classi <- bind_rows(classi,.id = 'Pairwise')
            
            x@classification <- classi
            x@log$classification <- date()
            return(x)
          }
)
