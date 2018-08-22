#' @importFrom FIEmspro valipars accest 
#' @importFrom stringr str_count 
#' @importFrom parallel makeCluster parLapply clusterExport stopCluster
#' @importFrom dplyr bind_rows

setMethod("classification", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == T) {
              startTime <- proc.time()
              cat(blue('Classification'),cli::symbol$continue,'\r',sep = '') 
            }
            parameters <- x@parameters@classification
            if (length(x@preTreated) > 0) {
              dat <- x@preTreated$Data
              cls <- x  %>%
                preTreatedInfo() %>%
                select(parameters$cls) %>%
                unlist()
            } else {
              dat <- x@rawData$Data
              cls <-  x  %>%
                rawInfo() %>%
                select(parameters$cls) %>%
                unlist()
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
            com <- combn(unique(as.character(cls)) %>% sort(),2)
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
            res.pair <- parLapply(clust,dat.pair,function(y,method,pars){
              res.meth <- lapply(method,function(z,dat,pars){
                accest(dat = dat[,-1],cl = factor(unlist(dat[,1])),clmeth = z,pars = pars)
              },dat = y,pars = pars)
              names(res.meth) <- method
              return(res.meth)
            },method = parameters$method,pars = par)
            stopCluster(clust)
            names(res.pair) <- com
            
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
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              cat(blue('Classification '),'\t\t',green(cli::symbol$tick),' ',elapsed,'\n',sep = '')
            }
            return(x)
          }
)
