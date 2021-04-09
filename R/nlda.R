## FIEmspro https://github.com/aberHRML/FIEmspro nlda functionality

#'@importFrom e1071 naiveBayes
#'@importFrom stats cov predict 

`nlda.default` <-
  function(dat,cl, prior=NULL,scale=FALSE,comprank = FALSE,...) 
  {
    if (missing(dat) || missing(cl)) 
      stop("data set or class are missing")
    dat <- as.matrix(dat)
    
    cl  <- as.factor(cl)
    if (any(table(cl) == 0)) stop("Can't have empty classes in cl.")
    
    if (nrow(dat) != length(cl)) stop("dat and cl don't match.")
    if (length(unique(cl)) < 2) 
      stop("Classification needs at least two classes.")
    if (any(is.na(dat)) || any(is.na(cl))) 
      stop("NA is not permitted in data set or class labels.")
    
    if(is.null(prior)){prior <- as.vector(table(cl)/length(cl))}
    if(is.null(names(prior))){names(prior) <- levels(cl)}
    
    pc  <- prcomp(dat,scale=scale)     
    
    rankmat <- max(1,ncol(pc$x)-1)
    
    if(comprank == TRUE)
      rankmat <- qr(cov(dat)*(dim(dat)[1]-1))$rank
    score <- pc$x[,1:rankmat,drop=FALSE]  
    
    g  <- nlevels(cl)
    mx <- apply(score,2,mean)
    t  <- matrix(0,nrow = rankmat,ncol=rankmat)
    W  <- matrix(0,nrow = rankmat,ncol=rankmat)
    for(j in 1:g){
      idx <- which(cl==levels(cl)[j])
      L   <- length(idx)
      K   <- score[idx,,drop=FALSE]         
      zz  <- apply(K,2,mean)
      A   <- K - t(matrix(rep(mx, L),length(mx),L))
      C   <- K - t(matrix(rep(zz, L),length(zz),L))
      t   <- t + t(A)%*%A
      W   <- W + t(C)%*%C
    }
    B <- t-W
    
    Ng    <- nrow(score)-g
    P     <- W/(Ng)
    eP    <- eigen(P)
    ord   <- sort.list(eP$values)
    V     <- sweep(
      eP$vectors[,ord,drop=FALSE],
      2, 
      sqrt(colSums(eP$vectors[,ord,drop=FALSE]^2)), 
      "/") 
    Dg    <- eP$values[ord]
    nDg   <- length(Dg)
    Dmean <- sum(diag(P))/nDg
    Dn    <- matrix(0,nDg,nDg)
    for(i in 1:nDg)
      Dn[i,i] <- max(c(Dg[i],Dmean))
    
    Wn    <- V%*%Dn%*%t(V)*Ng
    ratio <- solve(Wn)%*%B
    er    <- eigen(ratio)
    ev    <- Re(er$values)  
    ev[Im(er$values)>0] <- 0
    vec   <- Re(er$vectors)
    ord   <- sort.list(ev,decreasing=TRUE)
    vec   <- sweep(
      vec[,ord,drop=FALSE],
      2, 
      sqrt(colSums(vec[,ord,drop=FALSE]^2)),
      "/") 
    ev    <- ev[ord]
    maxg  <- min(c(g-1,dim(vec)[1]))
    vec   <- vec[,1:maxg] ## discriminant functions
    Tw    <- ev[1:maxg]
    names(Tw) <- paste("DF", 1:maxg, sep = "")
    
    ## get stats here
    flip   <- function(x) x[rev(seq_along(x))]
    n      <- dim(dat)[1]
    st     <- matrix(0,length(Tw),3)
    st[,1] <- round(Tw,3)
    st[,2] <- round(Tw*100/sum(Tw),3)
    st[,3] <- round(sqrt(Tw/(1+Tw)),3)
    st     <- as.data.frame(st)
    dimnames(st) <- list(
      paste("DF", 1:maxg, sep = ""),
      c("Eig","Perceig","Cancor"))
    
    res <- list()
    res$stats    <- st
    res$Tw       <- Tw
    res$rankmat  <- rankmat
    res$means    <- pc$center                        
    res$loadings <- pc$rotation[,1:rankmat,drop=FALSE] %*% 
      vec  ## discriminant functions with PCA
    
    colnames(res$loadings) <- paste("DF", 1:maxg, sep = "")  
    
    ## rotated data (projection)
    x <- sweep(dat, 2, res$means) %*% res$loadings
    
    ## group means based on the rotated data
    xmeans <- tapply(x, list(rep(cl,ncol(x)),col(x)), mean)
    dimnames(xmeans)[[2]] <- colnames(x)
    
    #if(type==1){
    #  mdist=as.matrix(dist(rbind(xmeans,x)))
    #  mdist=mdist[1:g,(g+1):ncol(mdist)]
    #  prob= (1-t(sweep(mdist,2,apply(mdist,2,sum),"/")))/(g-1)
    #  pred = apply(prob,1,which.max)
    #  pred <- factor(dimnames(prob)[[2]][pred], levels = levels(cl))
    #}
    #else{
    nbmod <- naiveBayes(data.frame(x),cl)
    prob <- predict(nbmod,data.frame(x),type="raw")
    pred <- apply(prob,1,which.max)
    pred <- factor(levels(cl)[pred], levels = levels(cl))
    #}
    res$x      <- x
    res$xmeans <- xmeans
    res$pred   <- pred
    res$cl     <- cl
    res$prior  <- prior
    res$conf   <- table(cl,pred)
    res$acc    <- round(sum(diag(res$conf))*100/nrow(dat),2)
    res$lev    <- levels(cl)     
    res$call   <- match.call()
    res$call[[1]]  <- as.name("nlda")
    class(res) <- "nlda"     
    
    return(res)
  }


nlda <- function (dat, ...) UseMethod ("nlda")
