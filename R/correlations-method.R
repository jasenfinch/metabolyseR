#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust
#' @importFrom dplyr filter bind_cols left_join rename select mutate
#' @importFrom parallel parApply makeCluster stopCluster
#' @importFrom tidyr gather
#' @importFrom tibble tibble

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
            
            cors <- tbl_df(data.frame(cors))
            cors <- bind_cols(Bin1 = rownames(cors),cors) %>% 
              gather('Bin2','r',-Bin1) %>% 
              filter(Bin1 != Bin2 & r != 0) %>%
              na.omit()
            
            clus <- makeCluster(parameters$nCores)
            cors <- parApply(clus,cors,1,function(x){
              x[1:2] <- c(x[1:2])[order(as.numeric(str_replace_all(x[1:2],'[:alpha:]','')))]
              return(x)
            })
            stopCluster(clus)
            cors <- data.frame(t(cors),stringsAsFactors = F)
            cors <- cors[!duplicated(cors[,1:2]),]
            cors$r <- as.numeric(cors$r)
            
            intensity <- tibble(Bin = names(colMeans(dat)), Intensity = colMeans(dat))
            
            cors <- tbl_df(cors) %>%
              left_join(intensity, by = c('Bin1' = 'Bin')) %>%
              rename(Intensity1 = Intensity) %>%
              left_join(intensity, by = c('Bin2' = 'Bin')) %>%
              rename(Intensity2 = Intensity) %>%
              mutate(log2IntensityRatio = log2(Intensity1/Intensity2)) %>%
              select(Bin1,Bin2,log2IntensityRatio,r)
              
            
            x@correlations <- cors
            x@log$correlations <- date()
            return(x)
          }
)