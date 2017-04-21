#' @rdname correlations
#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust
#' @importFrom stringr str_replace_all

setMethod("correlations", signature = "Analysis",
          function(object){
            parameters <- object@parameters$correlations
            
            cors <- rcorr(as.matrix(object@preTreated$Data))
            cors$P <- apply(cors$P,1,p.adjust,method = parameters$pAdjustMethod)
            cors$r[cors$P > parameters$corPvalue] <- 0
            cors$r[cors$r == 1] <- 0
            cors <- cors$r
            corsN <- cors
            corsN[corsN < 0] <- 0
            corsP <- cors
            corsP[corsP > 0] <- 0
            cors <- list(Negative = corsN, Positive = corsP)
            
            cors <- lapply(cors,function(x){
              s <- rowSums(abs(x))
              x <- x[s > 0,s > 0]
              n <- colnames(x)
              x <- metProc:::corLists(x)
              names(x) <- n
              return(x)
            })
           
            object@correlations <- cors
            object@log$correlations <- date()
            return(object)
          }
)