#' @rdname correlations
#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust
#' @importFrom stringr str_replace_all

setMethod("correlations", signature = "Analysis",
          function(x){
            parameters <- x@parameters@correlations
            
            cors <- rcorr(as.matrix(x@preTreated$Data))
            cors$P <- apply(cors$P,1,p.adjust,method = parameters$pAdjustMethod)
            cors$r[cors$P > parameters$corPvalue] <- 0
            cors$r[cors$r == 1] <- 0
            cors <- cors$r
            corsN <- cors
            corsN[corsN < 0] <- 0
            corsP <- cors
            corsP[corsP > 0] <- 0
            cors <- list(Negative = corsN, Positive = corsP)
            
            cors <- lapply(cors,function(y){
              s <- rowSums(abs(y))
              x <- x[s > 0,s > 0]
              n <- colnames(y)
              x <- metProc:::corLists(y)
              names(y) <- n
              return(y)
            })
           
            x@correlations <- cors
            x@log$correlations <- date()
            return(x)
          }
)