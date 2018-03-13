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

#' Linear Discriminant Analysis for High Dimensional Problems
#' 
#' Linear discriminant analysis for high dimensional problems. See details for
#' implementation.
#' 
#' A critical issue of applying linear discriminant analysis (LDA) is both the
#' singularity and instability of the within-class scatter matrix. In practice,
#' there are often a large number of features available, but the total number
#' of training patterns is limited and commonly less than the dimension of the
#' feature space. To tackle this issue, \code{nlda} combines principal
#' components analysis (PCA) and linear discriminant analysis (LDA) for the
#' classification problem.  Because the determination of the optimal number of
#' principal components representative for a dataset is not trivial and the
#' number of dimensions varies from one comparison to another introducing a
#' bias to the estimation of the separability measure, we have opted for a 2
#' steps procedure proposed in Thomaz, C. E. and Gillies, D. F. (2004): the
#' number of principal components to retain is equal to the rank of the
#' covariance matrix (usually number of training samples minus one) and the
#' within-class scatter matrix is replaced by a version where the less reliable
#' eigenvalues have been replaced. In addition to the proportion of explained
#' variance in each projection, the eigenvalue is a useful diagnostic quantity
#' (output \code{stats}).
#' 
#' @aliases nlda nlda.default nlda.formula print.nlda summary.nlda
#' print.summary.nlda
#' @usage nlda(dat, \dots{})
#' \method{nldadefault}(dat,cl,prior=NULL,scale=FALSE,comprank = FALSE,
#' \dots{})
#' 
#' \method{nldaformula}(formula, data = NULL, \dots{}, subset, na.action =
#' na.omit)
#' @param formula A formula of the form \code{groups ~ x1 + x2 + \dots{}} That
#' is, the response is the grouping factor and the right hand side specifies
#' the (non-factor) discriminators.
#' @param data Data frame from which variables specified in \code{formula} are
#' preferentially to be taken.
#' @param dat A matrix or data frame containing the explanatory variables if no
#' formula is given as the principal argument.
#' @param cl A factor specifying the class for each observation if no formula
#' principal argument is given.
#' @param prior The prior probabilities of class membership. If unspecified,
#' the class proportions for the training set are used. If present, the
#' probabilities should be specified in the order of the factor levels.
#' @param scale A logical value indicating whether or not PCA is scaled.
#' @param comprank A computation rank.
#' @param \dots Arguments passed to or from other methods.
#' @param subset An index vector specifying the cases to be used in the
#' training sample.
#' @param na.action A function to specify the action to be taken if \code{NA}s
#' are found. The default action is \code{na.omit}, which leads to rejection of
#' cases with missing values on any required variable. An alternative is
#' \code{na.fail}, which causes an error if \code{NA} cases are found.
#' @return An object of class \code{nlda} containing the following components:
#' \item{stats}{ The statistics based on the training data.  } \item{Tw}{ The
#' proportion of trace.  } \item{rankmat}{ The rank used for LDA.  }
#' \item{means}{ The means of training data.  } \item{loadings}{ A matrix of
#' the coefficients of linear discriminants.  } \item{x}{ The rotated data on
#' discriminant variables.  } \item{xmeans}{ The group means obtained from
#' training.  } \item{pred}{ The predicted class labels of training data.  }
#' \item{cl}{ The observed class labels of training data.  } \item{prior}{ The
#' prior probabilities used.  } \item{conf}{ The confusion matrix based on
#' training data.  } \item{acc}{ The accuracy rate of training data.  }
#' \item{lev}{ The levels of class.  } \item{call}{ The (matched) function
#' call.  }
#' @note This function may be given either a formula and optional data frame,
#' or a matrix and grouping factor as the first two arguments.
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}.
#' @seealso \code{\link{predict.nlda}}, \code{\link{plot.nlda}},
#' \code{\link{hca.nlda}}
#' @references Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied
#' Statistics with S.} Fourth edition.  Springer.
#' 
#' Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks}.
#' Cambridge University Press.
#' 
#' Thomaz, C. E. and Gillies, D. F. (2004) A Maximum Uncertainty LDA-based
#' approach for Limited Sample Size problems with application to Face
#' Recognition. \emph{Technical Report}.  Department of Computing, Imperial
#' College London.
#' 
#' Yang, J. and Yang J.-Y. (2003) Why can LDA be performed in PCA transformed
#' space? \emph{Pattern Recognition}, vol.36, 563 - 566.
#' @keywords classif
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  
#' 
#' ## define random training and test datasets
#' idx <- sample(1:nrow(dat), round((2/3)*nrow(dat)), replace=FALSE) 
#' train.dat  <- dat[idx,]
#' train.t    <- cl[idx]
#' test.dat   <- dat[-idx,]        
#' test.t     <- cl[-idx] 
#' 
#' ## build nlda on the training data
#' model    <- nlda(train.dat,train.t)
#' ## print summary
#' summary(model)
#' 
#' ## map samples on the first 2 DFs
#' plot(model,dimen=c(1,2),main = "Training data",abbrev = TRUE)
#' ## map samples on all the DFs
#' plot(model,main = "Training data",abbrev = TRUE)
#' 
#' ## predict test sample membership
#' pred.te  <- predict(model, test.dat)$class
#' ## confusion matrix and error rates
#' table(test.t,pred.te)
#' 
#' 
nlda <- function (dat, ...) UseMethod ("nlda")

## wll-20-06-2007: fix a bug


#' Plot Method for Class 'nlda'
#' 
#' Plots a set of data on one, two or more linear discriminants.
#' 
#' This function is a method for the generic function \code{plot()} for class
#' \code{nlda}. The behaviour is determined by the value of \code{dimen}. For
#' the length of \code{dimen} is greater than 2, a \code{pairs} plot is used.
#' For the length of \code{dimen} is equal to 2, a scatter plot is drawn.
#' Otherwise, a set of histograms or density plots are drawn.
#' 
#' @usage \method{plotnlda}(x, panel = panel.nlda, cex = 0.7, dimen, abbrev =
#' FALSE, \dots{})
#' @param x An object of class \code{nlda}.
#' @param panel The panel function used to plot the data.
#' @param cex Graphics parameter \code{cex} for labels on plots.
#' @param dimen The index of linear discriminants to be used for the plot.
#' @param abbrev Whether the group labels are abbreviated on the plots. If
#' \code{abbrev > 0} this gives \code{minlength} in the call to
#' \code{abbreviate}.
#' @param \dots Additional arguments to \code{plot}.
#' @author Wanchang Lin \email{wll@@aber.ac.uk} and David Enot
#' \email{dle@@aber.ac.uk}.
#' @seealso \code{\link{nlda}}, \code{\link{predict.nlda}},
#' \code{\link{hca.nlda}}
#' @keywords hplot
#' @examples
#' 
#' ## load abr1
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat <- preproc(abr1$pos , y=cl, method=c("log10","TICnorm"),add=1)[,110:500]  
#' 
#' ## build model on all the data available
#' model    <- nlda(dat,cl)
#' 
#' ## Plot second component versus first
#' plot(model,dimen=c(1,2),main = "Training data",abbrev = TRUE)
#' 
#' ## Pairwise scatterplots of several components 
#' plot(model,main = "Training data",abbrev = TRUE)
#' 
#' 
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
        MASS:::ldahist(xval[,1], g, ...)        
      } else {              ## Second component versus first
        xlab <- varlab[1]
        ylab <- varlab[2]
        MASS:::eqscplot(xval, xlab=xlab, ylab=ylab, type="n", ...)
        panel(xval[, 1], xval[, 2], ...)
      }
    } else {               ## Pairwise scatterplots of several components
      pairs(xval, labels = varlab, panel=panel, ...)
    }
    
    invisible(NULL)
    
  }

#' Classify Multivariate Observations by 'nlda'
#' 
#' Classify multivariate observations in conjunction with \code{nlda}, and also
#' project data onto the linear discriminants.
#' 
#' This function is a method for the generic function \code{predict()} for
#' class \code{nlda}. If \code{newdata} is omitted, the results of training
#' data in \code{nlda} object will be returned.
#' 
#' @usage \method{predictnlda}(object, newdata, dim2use = NULL, \dots{})
#' @param object Object of class \code{nlda}.
#' @param newdata A matrix or data frame of cases to be classified.
#' @param dim2use The dimension of rotated data set to be used in prediction.
#' @param \dots Arguments passed to or from other methods.
#' @return A list with components: \item{class}{ The predicted class (a
#' factor).  } \item{x}{ The projections of test data on discriminant
#' variables.  } \item{prob}{ The posterior probabilities for the predicted
#' classes.  } \item{xmeans}{ The group means obtained from training.  }
#' \item{dim2use}{ The dimension of rotated data set to be used in prediction.
#' }
#' @author David Enot \email{dle@@aber.ac.uk} and Wanchang Lin
#' \email{wll@@aber.ac.uk}.
#' @seealso \code{\link{nlda}}, \code{\link{plot.nlda}}
#' @keywords classif
#' @examples
#' 
#' 
#' data(abr1)
#' cl   <- factor(abr1$fact$class)
#' dat  <- abr1$pos
#' 
#' ## divide data as training and test data
#' idx <- sample(1:nrow(dat), round((2/3)*nrow(dat)), replace=FALSE) 
#' 
#' ## constrcuct train and test data 
#' train.dat  <- dat[idx,]
#' train.t    <- cl[idx]
#' test.dat   <- dat[-idx,]        
#' test.t     <- cl[-idx] 
#' 
#' ## apply NLDA
#' model    <- nlda(train.dat,train.t)
#' pred.te  <- predict(model, test.dat)
#' 
#' ## confusion matrix
#' table(test.t,pred.te$class)
#' 
#' 
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