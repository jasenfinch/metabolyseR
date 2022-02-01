## Based on FIEmspro https://github.com/aberHRML/FIEmspro nlda functionality

setGeneric('nlda',function(x,cls = 'class',prior = NULL,scale = FALSE,comprank = FALSE,...)
  standardGeneric('nlda'))

#' @importFrom e1071 naiveBayes 
#' @importFrom stats cov predict 
#' @importFrom methods as

setMethod('nlda',signature = 'AnalysisData',
          function(x,cls = 'class',prior=NULL,scale=FALSE,comprank = FALSE,...) {
            
            cl <- x %>% 
              clsExtract(cls) 
            
            if (is.numeric(cl))
              stop('Classes should not be numeric',call. = FALSE)
            
            cl <- factor(cl,levels = unique(cl))  
            
            if (any(table(cl) < 2)) {
              remove_classes <- cl %>% 
                table() %>%
                names() %>% 
                {.[table(cl) < 2]}
              
              x <- x %>%
                removeClasses(cls = cls,
                              classes = remove_classes)
              
              warning(str_c('Classes with a single replicate removed: ',
                            str_c(str_c('"',
                                        remove_classes,
                                        '"'),
                                  collapse = ', ')),
                      call. = FALSE)
              
              cl <- x %>% 
                clsExtract(cls) %>% 
                factor(levels = unique(.))
            }
            
            if (length(unique(cl)) < 2) 
              stop('More than 1 class needed for PC-LDA.',call. = FALSE)
            
            d <- x %>% 
              dat() %>% 
              as.matrix()
            
            if(is.null(prior)){prior <- as.vector(table(cl)/length(cl))}
            if(is.null(names(prior))){names(prior) <- levels(cl)}
            
            pc  <- prcomp(d,scale=scale)     
            
            rankmat <- max(1,ncol(pc$x)-1)
            
            if(comprank == TRUE)
              rankmat <- qr(cov(d)*(dim(d)[1]-1))$rank
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
            n      <- dim(d)[1]
            st     <- matrix(0,length(Tw),3)
            st[,1] <- round(Tw,3)
            st[,2] <- round(Tw*100/sum(Tw),3)
            st[,3] <- round(sqrt(Tw/(1+Tw)),3)
            st     <- as.data.frame(st)
            dimnames(st) <- list(
              paste("DF", 1:maxg, sep = ""),
              c("Eig","Perceig","Cancor"))
            
            res <- as(x,'LDA')
            res@stats    <- as_tibble(st)
            res@Tw       <- Tw
            res@rankmat  <- rankmat
            res@means    <- pc$center                        
            
            loadings <- pc$rotation[,1:rankmat,drop=FALSE] %*% vec  
            colnames(loadings) <- paste("DF", 1:maxg, sep = "")  
            x <- sweep(d, 2, res@means) %*% loadings
            
            ## group means based on the rotated data
            xmeans <- tapply(x, list(rep(cl,ncol(x)),col(x)), mean)
            dimnames(xmeans)[[2]] <- colnames(x)
            
            nbmod <- naiveBayes(data.frame(x),cl)
            prob <- stats::predict(nbmod,data.frame(x),type="raw")
            pred <- apply(prob,1,which.max)
            pred <- factor(levels(cl)[pred], levels = levels(cl))
            
            res@loadings <- as_tibble(loadings)
            res@x      <- as_tibble(x)
            res@xmeans <- as_tibble(xmeans)
            res@pred   <- pred
            res@cl     <- cl
            res@prior  <- prior
            res@conf   <- table(cl,pred)
            res@acc    <- round(sum(diag(res@conf))*100/nrow(d),2)
            res@lev    <- levels(cl)     
            res@call   <- match.call()
            res@call[[1]]  <- as.name("nlda")
            
            return(res)
          }
)
