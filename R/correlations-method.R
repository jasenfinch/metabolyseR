#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust na.omit
#' @importFrom dplyr filter bind_cols left_join rename select mutate
#' @importFrom parallel parApply makeCluster stopCluster
#' @importFrom tidyr gather
#' @importFrom tibble tibble as_tibble

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
            cors <- suppressWarnings(rcorr(cors,type = parameters$method))
            cors$P <- apply(cors$P,1,p.adjust,method = parameters$pAdjustMethod)
            cors$r[cors$P > parameters$corPvalue] <- 0
            cors <- cors$r
            cors[lower.tri(cors)] <- NA
            
            cors <- cors %>%
              as_tibble() %>%
              mutate(Feature1 = colnames(cors)) %>%
              gather('Feature2','r',-Feature1) %>%
              filter(Feature1 != Feature2, !is.na(r), r != 0)
            
            intensity <- tibble(Feature = names(colMeans(dat)), Intensity = colMeans(dat))
            
            cors <- cors %>%
              left_join(intensity, by = c('Feature1' = 'Feature')) %>%
              rename(Intensity1 = Intensity) %>%
              left_join(intensity, by = c('Feature2' = 'Feature')) %>%
              rename(Intensity2 = Intensity) %>%
              mutate(log2IntensityRatio = log2(Intensity1/Intensity2)) %>%
              select(Feature1,Feature2,log2IntensityRatio,r)
              
            
            x@correlations <- cors
            x@log$correlations <- date()
            return(x)
          }
)