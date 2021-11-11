#' Feature correlation analysis
#' @rdname correlations
#' @description Feature correlation analysis.
#' @param d S4 object of class `AnalysisData`
#' @param method correlation method. One of `pearson` or `spearman`.
#' @param pAdjustMethod p-value adjustment method. See `?p.adjust` for available methods.
#' @param corPvalue p-value cut-off threshold for significance
#' @param ... arguments to pass to specific method
#' @return A tibble containing results of significantly correlated features.
#' @details 
#' Correlation analyses can be used to identify associated features within data sets.
#' This can be useful to identifying clusters of related features that can be used to annotate metabolites within data sets.
#' All features are compared and the returned table of correlations are p-value thresholded using the specified cut-off.
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#' 
#' correlations(d)
#' @export

setGeneric("correlations", function(d,...)
  standardGeneric("correlations"))

#' @rdname correlations

setMethod('correlations',signature = 'AnalysisData',
          function(d, 
                   method = 'pearson', 
                   pAdjustMethod = 'bonferroni', 
                   corPvalue = 0.05){
            doCorrelations(d, 
                           method = method,
                           pAdjustMethod = pAdjustMethod, 
                           corPvalue = corPvalue)
          })

#' @rdname correlations

setMethod("correlations", signature = "Analysis",
          function(d){
            verbose <- d@log$verbose
            if (verbose == TRUE) {
              startTime <- proc.time()
              message(blue('Correlations '),
                      cli::symbol$continue,
                      '\r',
                      appendLF = FALSE) 
            }
            
            params <- d %>%
              parameters() %>%
              parameters('correlations')
            
            if (dat(d,type = 'pre-treated') %>% nrow() > 0) {
              da <- d %>% 
                preTreated()
            } else {
              da <- d %>%
                raw()
            }
            
            rs <- correlations(da,
                               params$method,
                               params$pAdjustMethod,
                               params$corPvalue)
            
            d@correlations <- rs
            d@log$correlations <- date()
            
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
            return(d)
          }
)

#' @importFrom Hmisc rcorr
#' @importFrom stats p.adjust na.omit
#' @importFrom dplyr filter bind_cols left_join rename select mutate distinct
#' @importFrom tidyr gather
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_df

doCorrelations <- function(d, 
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
  
  d <-  d %>%
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
    mutate(`|r|` = abs(r),
           log2IntensityRatio = log2(Intensity1/Intensity2)) %>%
    select(Feature1,Feature2,log2IntensityRatio,r,`|r|`,p,n)  %>% 
    na.omit() %>% 
    arrange(desc(`|r|`))
  
  return(rs)
}
