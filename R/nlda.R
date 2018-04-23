## FIEmspro https://github.com/aberHRML/FIEmspro nlda functionality

nlda.formula <-
  function (formula, data = NULL, ..., subset, na.action = na.omit)
  {
    call <- match.call()
    if (!inherits(formula, "formula"))
      stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    if (identical(class(eval.parent(m$data)), "matrix"))
      m$data <- as.data.frame(eval.parent(m$data))
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    m$na.action <- na.action
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    x <- model.matrix(Terms, m)
    y <- model.extract(m, "response")
    attr(x, "na.action") <- attr(y, "na.action") <- attr(m, "na.action")
    
    ret <- nlda.default (x, y, ..., na.action = na.action)
    
    ret$call <- call
    ret$call[[1]] <- as.name("nlda")
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
      ret$na.action <- attr(m, "na.action")
    class(ret) <- c("nlda.formula", class(ret))
    return (ret)
  }

## wll-20-06-2006: check if cl has empty class
#'@importFrom e1071 naiveBayes

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
    
    if(is.null(prior)){prior=as.vector(table(cl)/length(cl))}
    if(is.null(names(prior))){names(prior)=levels(cl)}
    
    pc  <- prcomp(dat,scale=scale)     
    
    rankmat <- max(1,ncol(pc$x)-1)
    
    if(comprank == TRUE)
      rankmat = qr(cov(dat)*(dim(dat)[1]-1))$rank
    score   = pc$x[,1:rankmat,drop=F]  
    
    g  = nlevels(cl);
    mx = apply(score,2,mean);
    T  <- matrix(0,nrow = rankmat,ncol=rankmat)
    W  <- matrix(0,nrow = rankmat,ncol=rankmat)
    for(j in 1:g){
      idx = which(cl==levels(cl)[j])
      L   = length(idx)
      K   = score[idx,,drop=F]         
      zz  = apply(K,2,mean)
      A   = K - t(matrix(rep(mx, L),length(mx),L))
      C   = K - t(matrix(rep(zz, L),length(zz),L))
      T   = T + t(A)%*%A
      W   = W + t(C)%*%C
    }
    B = T-W;
    
    Ng    = nrow(score)-g
    P     = W/(Ng);
    eP    = eigen(P);
    ord   = sort.list(eP$values)
    V     = sweep(eP$vectors[,ord,drop=F], 2, sqrt(colSums(eP$vectors[,ord,drop=F]^2)), "/") 
    Dg    = eP$values[ord];
    nDg   = length(Dg);
    Dmean = sum(diag(P))/nDg;
    Dn    = matrix(0,nDg,nDg);
    for(i in 1:nDg)
      Dn[i,i] = max(c(Dg[i],Dmean))
    
    Wn    = V%*%Dn%*%t(V)*Ng
    ratio = solve(Wn)%*%B;
    er    = eigen(ratio);
    ev    = Re(er$values);  
    ev[Im(er$values)>0]=0;
    vec   = Re(er$vectors)
    ord   = sort.list(ev,decreasing=T)
    vec   = sweep(vec[,ord,drop=F], 2, sqrt(colSums(vec[,ord,drop=F]^2)), "/") 
    ev    = ev[ord];
    maxg  = min(c(g-1,dim(vec)[1]));
    vec   = vec[,1:maxg]                         ## discriminant functions
    Tw    = ev[1:maxg]
    names(Tw) <- paste("DF", 1:maxg, sep = "")
    
    ## get stats here
    flip   <- function(x) x[length(x):1]
    n      = dim(dat)[1]
    st     = matrix(0,length(Tw),3)
    st[,1] = round(Tw,3)
    st[,2] = round(Tw*100/sum(Tw),3)
    st[,3] = round(sqrt(Tw/(1+Tw)),3)
    st     = as.data.frame(st)
    dimnames(st) <- list(paste("DF", 1:maxg, sep = ""),c("Eig","Perceig","Cancor"))
    
    res <- list()
    res$stats    = st
    res$Tw       = Tw
    res$rankmat  = rankmat
    res$means     = pc$center                        
    res$loadings = pc$rotation[,1:rankmat,drop=F]%*%vec  ## discriminant functions with PCA
    
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
    nbmod=naiveBayes(data.frame(x),cl)
    prob=predict(nbmod,data.frame(x),type="raw")
    pred = apply(prob,1,which.max)
    pred <- factor(levels(cl)[pred], levels = levels(cl))
    #}
    res$x      = x
    res$xmeans = xmeans
    res$pred   = pred
    res$cl     = cl
    res$prior  = prior
    res$conf   = table(cl,pred)
    res$acc    = round(sum(diag(res$conf))*100/nrow(dat),2)
    res$lev        = levels(cl)     
    res$call       = match.call()
    res$call[[1]]  = as.name("nlda")
    class(res) <- "nlda"     
    
    return(res)
  }


nlda <- function (dat, ...) UseMethod ("nlda")

## wll-20-06-2007: fix a bug

`plot.nlda` <-
  function(x, panel = panel.nlda, cex=0.7, dimen, abbrev = FALSE, ...)
  {
    panel.nlda <- function(x, y, ...) {
      text(x, y, as.character(g.nlda), cex=tcex, col=unclass(g),...)
    }
    eig.val <- function(x, dimen) { 
      tmp <- rownames(x$stats)
      tmp <- tmp[dimen]
      eig <- x$stats[dimen,1]   ## Eigenvalues
      per <- x$stats[dimen,2]   ## Percentage
      tmp <- paste(tmp, " (", format(eig, digits = 2, trim = TRUE), ", ",
                   format(per, digits = 2, trim = TRUE),"%)", sep = "")
    }
    
    xval <- x$x
    g    <- x$cl
    
    if(abbrev) levels(g) <- abbreviate(levels(g), abbrev)
    assign("g.nlda", g)
    assign("tcex", cex)
    
    if (missing(dimen)){
      dimen <- seq(along=colnames(xval))
    } else {
      if (!all(dimen %in% c(1:ncol(xval)))){  
        ## stop("dimen is not valid")
        warning("dimen is not valid. Use default plotting.")
        dimen <- seq(along=colnames(xval))
      }
    }
    
    xval   <- xval[, dimen, drop=FALSE]
    varlab <- eig.val(x, dimen)
    nDimen <- length(dimen)
    
    if (nDimen <= 2) {
      if (nDimen == 1) {    ## One component
        ldahist(xval[,1], g, ...)        
      } else {              ## Second component versus first
        xlab <- varlab[1]
        ylab <- varlab[2]
        eqscplot(xval, xlab=xlab, ylab=ylab, type="n", ...)
        panel(xval[, 1], xval[, 2], ...)
      }
    } else {               ## Pairwise scatterplots of several components
      pairs(xval, labels = varlab, panel=panel, ...)
    }
    
    invisible(NULL)
    
  }

`predict.nlda` <-
  function(object, newdata, dim2use=NULL,...)
  {
    if(!inherits(object, "nlda")) stop("object not of class \"nlda\"")
    if (missing(newdata)) {
      return(list(class = object$pred, x = object$x, xmeans=object$xmeans,
                  conf = object$conf, acc = object$acc))
    }
    if(is.null(dim(newdata)))
      dim(newdata) <- c(1, length(newdata))  ## a row vector
    newdata <- as.matrix(newdata)		   
    if(ncol(newdata) != nrow(object$loadings)) stop("wrong number of variables")
    
    g <- length(object$lev)
    
    ## rotated data (projection)
    x <- sweep(newdata, 2, object$means) %*% object$loadings
    
    if(is.null(dim2use))
      dim2use <- ncol(x)
    
    #if(type==1){
    
    #  mdist=as.matrix(dist(rbind(object$xmeans[,1:dim2use,drop=F],x[,1:dim2use,drop=F])))
    #  mdist=mdist[1:g,(g+1):ncol(mdist)]
    #  prob=(1-t(sweep(mdist,2,apply(mdist,2,sum),"/")))/g
    #  pred = apply(prob,1,which.max)
    #  pred <- factor(dimnames(prob)[[2]][pred], levels = object$lev)
    #}
    #else{
    
    nbmod=naiveBayes(data.frame(object$x[,1:dim2use,drop=F]),object$cl)
    prob=predict(nbmod,data.frame(x[,1:dim2use,drop=F]),type="raw")
    pred = apply(prob,1,which.max)
    pred <- factor(levels(object$cl)[pred], levels = object$lev)
    #}
    
    ##  list(class = pred, prob = prob , x = x,  xmeans = object$xmeans, dim2use=dim2use)
    list(class = pred, prob = prob,posterior = prob , x = x,  xmeans = object$xmeans, dim2use=dim2use)
    
  }

`print.nlda` <-
  function(x, ...)
  {
    cat("\nCall:\n", deparse(x$call), "\n")
    cat("\nStatistics of training:\n")
    print(x$stats, ...)
    ## cat("\nCoefficients of linear discriminants:\n")
    ## print(x$loadings, ...)
    ## cat("\nProportion of trace:\n")
    ## print(x$Tw, ...)
    cat("\nConfusion matrix of training data:\n")
    print(x$conf)
    ## cat("\nAccurary rate of training data:\n")
    ## print(x$acc)
    invisible(x)
  }

`print.summary.nlda` <-
  function (x, ...) {
    print.nlda(x)
    
    ## cat("\nNumber of Classes: ", length(x$lev), "\n\n")
    ## cat("Levels:", if(is.numeric(x$lev)) "(as integer)", "\n", x$lev)
    ## cat("\n\n")
  }

`summary.nlda` <-
  function(object, ...)
    structure(object, class="summary.nlda")