#' @importFrom randomForest randomForest
#' @importFrom stats oneway.test t.test kruskal.test

fsMethods <- function(method = NULL, description = F){
  methods <- list(
    
    fs.rf = function(dat,nreps = 100){
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
        FPR <- sapply(sort(unique(meas)),selectionFrequencyFPR,K = kval,Tr = 1000,Ft = length(meas))
        FPR.pos <- match(meas,sort(unique(meas)))
        for (i in 1:length(FPR)) {
          FPR.pos[which(FPR.pos == i)] <- FPR[i]
        }
        names(FPR.pos) <- SFtable$SF
        FPR.pos <- data.frame(Feature = names(FPR.pos),Score = FPR.pos,stringsAsFactors = F)
        return(FPR.pos)
      },dat = dat)
      f <- res[[1]]$Feature
      res <- lapply(res,function(x){
        return(x$Score)
      })
      res <- as.data.frame(res)
      res <- rowMeans(res)
      res <- data.frame(Feature = f,Score = res)
      return(res)
    },
    
    fs.anova = function(dat,pAdjust = 'bonferroni'){
      cls <- factor(dat$cls)
      dat <- dat[,-1]
      res <- apply(dat,2,function(x,cls){
        r <- oneway.test(x ~ cls)
        return(r$p.value)
      },cls = cls)
      res <- data.frame(Feature = names(res), Score = res)
      res$Score <- p.adjust(res$Score,method = pAdjust)
      return(res)
    },
    
    fs.ttest = function(dat,pAdjust = 'bonferroni'){
      cls <- factor(dat$cls)
      dat <- dat[,-1]
      res <- apply(dat,2,function(x,cls){
        r <- t.test(x ~ cls)
        return(r$p.value)
      },cls = cls)
      res <- data.frame(Feature = names(res), Score = res)
      res$Score <- p.adjust(res$Score,method = pAdjust)
      return(res)
    },
    
    fs.kruskal = function(dat,pAdjust = 'bonferroni'){
      cls <- factor(dat$cls)
      dat <- dat[,-1]
      res <- apply(dat,2,function(x,cls){
        r <- kruskal.test(x ~ cls)
        return(r$p.value)
      },cls = cls)
      res <- data.frame(Feature = names(res), Score = res)
      res$Score <- p.adjust(res$Score,method = pAdjust)
      return(res)
    }
  )
  
  descriptions = list(
    fs.rf = list(description = 'Random Forest using selection frequency based false positive rate for variable importance',
                 arguments = c(nreps = 'number of replications')),
    fs.anova = list(description = 'One-way ANOVA', 
                    arguments = c(pAdjust = 'method for multiple testing p value correction')),
    fs.ttest = list(description = 'Welch t-test', 
                    arguments = c(pAdjust = 'method for multiple testing p value correction')),
    fs.kruskal = list(description = 'Kruskal-Wallis Rank Sum Test', 
                      arguments = c(pAdjust = 'method for multiple testing p value correction'))
  )
  
  if (description == F) {
    if (is.null(method)) {
      method <- methods
    } else {
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      method <- descriptions[[method]]
    }
  }
  return(method)
}
