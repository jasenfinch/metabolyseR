#' plotExplanatoryHeatmap
#' @rdname plotExplanatoryHeatmap
#' @description plot a heatmap of explanatory features
#' @param analysis object of class Analysis containing analysis results
#' @param method results of feature selection method to use
#' @param threshold score threshold to use for specifying explantory features
#' @param pairwises optional vector specifying pairwise comparisons to extract
#' @param distanceMeasure distance measure to use for clustering. See details.
#' @param clusterMethod clustering method to use. See details
#' @param colour heatmap colour to use
#' @details 
#' Options for distance measures are as for \code{dist()}.
#' Clustering methods are as given for \code{hclust()}.
#' @seealso \link{dist} \link{hclust}
#' @importFrom stats dist hclust
#' @importFrom ggdendro dendro_data 
#' @importFrom ggplot2 geom_tile scale_fill_gradient theme_minimal labs
#' @examples \dontrun{
#' 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat','featureSelection'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p) 
#' plotExplanatoryHeatmap(analysis)
#' }
#' @export

setMethod('plotExplanatoryHeatmap',signature = 'Analysis',
          function(analysis, method = 'fs.rf', threshold = 0.01, pairwises = NULL, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', colour = ggthemes::ptol_pal()(1)){
            dat <- preTreatedData(analysis)
            info <- preTreatedInfo(analysis)
            
            cls <- analysis@parameters@featureSelection$cls
            
            info <- info %>%
              select(Class = cls)
            
            feat <- featureSelectionResults(analysis) %>%
              filter(Method == method,Pvalue < threshold)
            
            if (!is.null(pairwises)) {
              feat <- feat %>%
                filter(Pairwise %in% pairwises)
            }
            
            classes <- feat %>%
              select(Pairwise) %>%
              unique() %>%
              unlist() %>%
              map(~{str_split(.,'~')[[1]]}) %>%
              unlist() %>%
              unique()
            
            feat <- feat %>%
              select(Feature) %>%
              unique() %>%
              unlist()
            
            dat <- dat %>%
              bind_cols(info) %>%
              gather('Feature','Intensity',-Class) %>%
              filter(Class %in% classes,Feature %in% feat) %>%
              group_by(Class,Feature) %>%
              summarise(Intensity = mean(Intensity))
            
            sums <- dat %>%
              group_by(Feature) %>%
              summarise(Total = max(Intensity))
            
            dat <- dat %>%
              left_join(sums,by = c('Feature' = 'Feature')) %>%
              mutate(`Relative Intensity` = Intensity/Total)
            
            clusters <- dat %>%
              select(-Intensity,-Total) %>%
              spread(Class,`Relative Intensity`)
            suppressWarnings(rownames(clusters) <- clusters$Feature)
            clusters <- clusters %>%
              select(-Feature) %>%
              dist(distanceMeasure) %>%
              hclust(clusterMethod) %>%
              dendro_data()
            clusters <- clusters$labels$label
            
            dat <- dat %>%
              tbl_df() %>%
              mutate(Feature = factor(Feature,levels = clusters),
                     Class = factor(Class))
            
            pl <- dat %>%
              ggplot(aes(x = Class,y = Feature,fill = `Relative Intensity`)) +
              geom_tile() +
              scale_fill_gradient(low = 'white', high = colour) +
              theme_minimal(base_size = 8) +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    axis.text.x = element_text(angle = 30,hjust = 1)
                    ) +
              ggtitle(str_c('Heat map of explanatory features for method ',method)) +
              labs(caption = str_c('Explanatory features had a P value below a threshold of ',threshold,'.'),
                   fill = 'Relative\nIntensity')
            
            pl
          }
)
