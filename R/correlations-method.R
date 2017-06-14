#' @rdname correlations
#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust
#' @importFrom reshape2 melt
#' @importFrom dplyr filter

setMethod("correlations", signature = "Analysis",
          function(x){
            parameters <- x@parameters@correlations
            
            cors <- as.matrix(x@preTreated$Data)
            cors[cors == 0] <- NA
            cors <- rcorr(cors)
            cors$P <- apply(cors$P,1,p.adjust,method = parameters$pAdjustMethod)
            cors$r[cors$P > parameters$corPvalue] <- 0
            cors <- cors$r
            
            cors <- data.frame(rownames(cors),cors,stringsAsFactors = F) 
            cors <- melt(cors)
            colnames(cors) <- c('Bin1','Bin2','r')
            cors <- filter(cors, Bin1 != Bin2)
            cors <- cors[cors$r != 0,] 
            cors <- na.omit(cors)
            
            x@correlations <- cors
            x@log$correlations <- date()
            return(x)
          }
)