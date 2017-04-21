#' @rdname featureSelection
#' @importFrom randomForest randomForest
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
            res.pair <- parLapply(clust,dat.pair,function(x,nreps){
              res <- lapply(1:nreps,function(y,dat){
                cls <- factor(dat$cls)
                dat <- dat[,-1]
                res <- randomForest::randomForest(dat,y = cls,importance = T, keep.forest = T,ntree = 1000)
                SF <- res$forest$bestvar
                SFtable <- data.frame(table(SF))
                SFtable <- SFtable[-which(as.numeric(as.character(SFtable$SF)) == 0),]
                SFtable$SF <- colnames(dat)[as.numeric(as.character(SFtable$SF))]
                SFtable <- rbind(SFtable,data.frame(SF = colnames(dat)[!(colnames(dat) %in% SFtable$SF)],Freq = rep(0,length(which(!(colnames(dat) %in% SFtable$SF))))))
                SFtable <- SFtable[order(SFtable$SF),]
                kval <- round(mean(apply(res$forest$nodestatus,2,function(x){length(which(x == 1))})),0)
                meas <- SFtable$Freq
                names(meas) <- SFtable$SF
                FPR <- sapply(sort(unique(meas)),metProc:::selectionFrequencyFPR,K = kval,Tr = 1000,Ft = length(meas))
                FPR.pos <- match(meas,sort(unique(meas)))
                for (i in 1:length(FPR)) {
                  FPR.pos[which(FPR.pos == i)] <- FPR[i]
                }
                names(FPR.pos) <- SFtable$SF
                FPR.pos <- data.frame(Feature = names(FPR.pos),Score = FPR.pos,stringsAsFactors = F)
                return(FPR.pos)
              },dat = x)
              f <- res[[1]]$Feature
              res <- lapply(res,function(x){
                return(x$Score)
              })
              res <- as.data.frame(res)
              res <- rowMeans(res)
              res <- data.frame(Feature = f,Score = res)
              return(res)
            },nreps = parameters$nreps)
            stopCluster(clust)
            names(res.pair) <- com
            f <- res.pair[[1]]$Feature
            res.pair <- lapply(res.pair,function(x){x$Score})
            res.pair <- as.data.frame(res.pair)
            colnames(res.pair) <- com
            rownames(res.pair) <- f
            
            object@featureSelection <- res.pair
            object@log$featureSelection <- date()
            return(object)
          }
)