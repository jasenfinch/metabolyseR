#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust na.omit
#' @importFrom dplyr filter bind_cols left_join rename select mutate
#' @importFrom parallel parApply makeCluster stopCluster
#' @importFrom tidyr gather
#' @importFrom tibble tibble as_tibble

setMethod("correlations", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == T) {
              startTime <- proc.time()
              cat(blue('Correlations'),cli::symbol$continue,'\r',sep = '') 
            }
            
            parameters <- x@parameters@correlations
            if (length(x@preTreated) > 0) {
              dat <- x %>% 
                preTreatedData()
            } else {
              dat <- x %>%
                rawData()
            }
            
            dat <-  dat %>%
              as.matrix()
            
            intensity <- tibble(Feature = names(colMeans(dat)), Intensity = colMeans(dat))
            
            dat[dat == 0] <- NA
            cors <- suppressWarnings(rcorr(dat,,type = parameters$method))
            
            ps <- cors$P %>%
              as_tibble() %>%
              map_df(p.adjust,method = parameters$pAdjustMethod,n = nrow(.) - 1) %>%
              mutate(Feature1 = colnames(.)) %>%
              gather('Feature2','p',-Feature1) %>%
              distinct()
            
            ns <- cors$n %>%
              as_tibble() %>%
              mutate(Feature1 = colnames(.)) %>%
              gather('Feature2','n',-Feature1) %>%
              distinct()
            
            rs <- cors$r %>%
              as_tibble() %>%
              mutate(Feature1 = colnames(.)) %>%
              gather('Feature2','r',-Feature1) %>%
              distinct() %>%
              bind_cols(ps %>% select(p),
                        ns %>% select(n)) %>%
              filter(Feature1 != Feature2,p < parameters$corPvalue,n > 2) %>% 
              left_join(intensity, by = c('Feature1' = 'Feature')) %>%
              rename(Intensity1 = Intensity) %>%
              left_join(intensity, by = c('Feature2' = 'Feature')) %>%
              rename(Intensity2 = Intensity) %>%
              mutate(log2IntensityRatio = log2(Intensity1/Intensity2)) %>%
              select(Feature1,Feature2,log2IntensityRatio,r,p,n)  
            
            x@correlations <- rs
            x@log$correlations <- date()
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              cat(blue('Correlations '),'\t\t',green(cli::symbol$tick),' ',elapsed,'\n',sep = '')
            }
            return(x)
          }
)