#' plotClassificationDendrogram
#' @rdname plotClassificationDendrogram
#' @description plot a dendrogram of classifcation results
#' @param analysis object of class Analysis containing analysis results
#' @param method results of classifier to plot
#' @param measure model measure to use
#' @param clusterMethod clustering method to use
#' @importFrom stringr str_split
#' @importFrom tidyr spread
#' @importFrom ggdendro ggdendrogram
#' @importFrom stats as.dist
#' @examples \dontrun{
#' 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat','classification'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p) 
#' plotClassificationDendrogram(analysis)
#' }
#' @export

setMethod('plotClassificationDendrogram',signature = 'Analysis',
          function(analysis,method = 'randomForest', measure = 'Margin', clusterMethod = 'ward.D2'){
            classification <- classificationResults(analysis) %>%
              group_by(Pairwise,Method,Measure) %>%
              summarise(Mean = mean(Value), SD = sd(Value),SE = sd(Value)/sqrt(n())) %>%
              tbl_df() %>%
              filter(Method == method, Measure == measure) %>%
              select(Pairwise,Mean)
            
            pairs <- classification %>%
              select(Pairwise) %>%
              unlist() %>%
              map(~{str_split(.,'~')[[1]]}) %>%
              bind_cols() %>%
              t() %>%
              as_tibble() %>%
              rename(P1 = V1, P2 = V2)
            
            pairs1 <- pairs %>%
              rename(P2 = P1, P1 = P2)
            
            pairs <- pairs %>%
              bind_rows(pairs1) %>%
              bind_cols(bind_rows(classification,classification))
            
            classification <- pairs %>%
              select(-Pairwise) %>%
              spread(P1,Mean)
            
            rownames(classification) <- classification$P2
            
            classification %>%
              select(-P2) %>%
              as.dist() %>%
              hclust(method = clusterMethod) %>%
              ggdendrogram(rotate = T)
          }
)