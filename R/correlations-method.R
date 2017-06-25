#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust
#' @importFrom reshape2 melt
#' @importFrom dplyr filter

setMethod("correlations", signature = "Analysis",
          function(x){
            parameters <- x@parameters@correlations
            if (length(x@preTreated) > 0) {
              dat <- x@preTreated$Data
            } else {
              dat <- x@rawData$Data
            }
            cors <- as.matrix(dat)
            cors[cors == 0] <- NA
            cors <- suppressWarnings(rcorr(cors))
            cors$P <- apply(cors$P,1,p.adjust,method = parameters$pAdjustMethod)
            cors$r[cors$P > parameters$corPvalue] <- 0
            cors <- cors$r
            
            cors <- data.frame(rownames(cors),cors,stringsAsFactors = F) 
            cors <- suppressMessages(melt(cors))
            colnames(cors) <- c('Bin1','Bin2','r')
            cors <- filter(cors, Bin1 != Bin2)
            cors <- cors[cors$r != 0,] 
            cors <- na.omit(cors)
            cors <- apply(cors,1,function(x){
              x[1:2] <- c(x[1:2])[order(as.numeric(str_replace_all(x[1:2],'[:alpha:]','')))]
              return(x)
            })
            cors <- data.frame(t(cors),stringsAsFactors = F)
            cors <- cors[!duplicated(cors[,1:2]),]
            x@correlations <- tbl_df(cors)
            x@log$correlations <- date()
            return(x)
          }
)