
doCorrelations <- function(x, 
                           method = 'pearson', 
                           pAdjustMethod = 'bonferroni', 
                           corPvalue = 0.05)
{
  
  methods <- eval(formals(rcorr)$type)
  if (!(method %in% methods)) {
    methods <- str_c('"',methods,'"')
    stop(str_c('Argument "method" should be one of ',
               str_c(methods,collapse = ', '),'.'),
         call. = FALSE)
  }
  
  if (!(pAdjustMethod %in% p.adjust.methods)) {
    methods <- str_c('"',p.adjust.methods,'"')
    stop(str_c('Argument "pAdjustMethod" should be one of ',
               str_c(methods,collapse = ', '),'.'),
         call. = FALSE)
  }
  
  if (!is.numeric(corPvalue) | length(corPvalue) > 1) {
    stop('Argument "corPvalue" should be a single numeric value.',
         call. = FALSE)
  }
  
  d <-  x %>%
    dat() %>%
    as.matrix()
  
  intensity <- tibble(Feature = names(colMeans(d)), Intensity = colMeans(d))
  
  d[d == 0] <- NA
  cors <- suppressWarnings(rcorr(d,type = method))
  
  cors$r[lower.tri(cors$r)] <- NA
  cors$n[lower.tri(cors$n)] <- NA
  cors$P[lower.tri(cors$P)] <- NA
  
  ps <- cors$P %>%
    as_tibble() %>%
    map_df(p.adjust,method = pAdjustMethod,n = nrow(.) - 1) %>%
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
    filter(Feature1 != Feature2,p < corPvalue,n > 2) %>% 
    left_join(intensity, by = c('Feature1' = 'Feature')) %>%
    rename(Intensity1 = Intensity) %>%
    left_join(intensity, by = c('Feature2' = 'Feature')) %>%
    rename(Intensity2 = Intensity) %>%
    mutate(log2IntensityRatio = log2(Intensity1/Intensity2)) %>%
    select(Feature1,Feature2,log2IntensityRatio,r,p,n)  %>% 
    na.omit()
  
  return(rs)
}

#' correlations
#' @rdname correlations
#' @description Calculate variable correlations.
#' @param x S4 object of class AnalysisData
#' @param method correlation method. 
#' One of \code{'pearson'} or \code{'spearman'}.
#' @param pAdjustMethod p-value adjustment method. 
#' See \code{?p.adjust} for availabl methods.
#' @param corPvalue p-value cutoff threshold for significance
#' @param ... arguments to pass to specific method
#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust na.omit
#' @importFrom dplyr filter bind_cols left_join rename select mutate distinct
#' @importFrom tidyr gather
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_df
#' @export

setMethod('correlations',signature = 'AnalysisData',
          function(x, 
                   method = 'pearson', 
                   pAdjustMethod = 'bonferroni', 
                   corPvalue = 0.05){
            doCorrelations(x, 
                           method = method,
                           pAdjustMethod = pAdjustMethod, 
                           corPvalue = corPvalue)
          })

#' @rdname correlations

setMethod("correlations", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == TRUE) {
              startTime <- proc.time()
              message(blue('Correlations '),
                      cli::symbol$continue,
                      '\r',
                      appendLF = FALSE) 
            }
            
            params <- x %>%
              parameters() %>%
              parameters('correlations')
            
            if (dat(x,type = 'pre-treated') %>% nrow() > 0) {
              d <- x %>% 
                preTreated()
            } else {
              d <- x %>%
                raw()
            }
            
            rs <- correlations(d,
                               params$method,
                               params$pAdjustMethod,
                               params$corPvalue)
            
            x@correlations <- rs
            x@log$correlations <- date()
            
            if (verbose == TRUE) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              message(blue('\rCorrelations '),
                      '\t',
                      green(cli::symbol$tick),
                      ' ',
                      elapsed)
            }
            return(x)
          }
)
