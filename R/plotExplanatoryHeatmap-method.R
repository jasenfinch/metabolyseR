#' @importFrom ggplot2 ggplot aes_string theme scale_y_discrete geom_segment scale_x_reverse scale_y_continuous unit

heatmapClasses <- function(pl, x, threshold, distanceMeasure, clusterMethod, featureNames,dendrogram){
  pl %>%
    map(~{
      r <- .
      pred <- r$Response[1]
      
      classes <- r$Comparison %>%
        unique() %>%
        str_split('~') %>%
        unlist() %>%
        unique()
      
      feat <- r$Feature %>%
        unique()
      
      d <- x@data %>%
        keepClasses(cls = pred,
                    classes = classes) %>%
        keepFeatures(features = feat)
      
      d <- d %>%
        sinfo() %>%
        select(all_of(pred)) %>%
        bind_cols(d %>%
                    dat()) %>%
        gather('Feature','Intensity',-1) %>%
        group_by_at(c(pred,'Feature')) %>%
        summarise(Intensity = mean(Intensity),.groups = 'drop')
      
      sums <- d %>%
        group_by(Feature) %>%
        summarise(Total = max(Intensity),.groups = 'drop')
      
      d <- d %>%
        left_join(sums,by = c('Feature')) %>%
        mutate(`Relative Intensity` = Intensity / Total)
      
      suppressWarnings({
        dend <- d %>%
          select(-Intensity,-Total) %>%
          spread(1,`Relative Intensity`) %>%
          data.frame(check.names = FALSE) %>%
          set_rownames(.$Feature) %>%
          select(-Feature) %>%
          dist(distanceMeasure) %>%
          hclust(clusterMethod) %>%
          dendro_data()  
      })
      
      clusters <- dend$labels$label
      
      d <- d %>%
        mutate(Feature = factor(Feature,levels = clusters)) %>%
        mutate_at(pred,factor)
      
      caption <- str_c('Explanatory features had a P value below a threshold of ',threshold,'.')
      
      if (length(feat) > 3000) {
        caption <- str_c(caption,'\n','Number of features capped at top 3000.')
      }
      low <- 'white'
      high <- "#F21A00"
      
      plo <- d %>%
        ggplot(aes_string(x = pred,y = 'Feature',fill = '`Relative Intensity`')) +
        geom_tile(colour = 'black') +
        scale_fill_gradient(low = low, high = high,limits=c(0,1)) +
        scale_y_discrete(expand = c(0,0),position = 'right') +
        theme_minimal(base_size = 8) +
        labs(title = pred,
             caption = caption,
             fill = 'Relative\nIntensity')
      if (isTRUE(featureNames)) {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold'),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      } else {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold'),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                axis.text.y = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      }
      
      if (isTRUE(dendrogram)) {
        dend_plot <- ggplot() +
          geom_segment(data = dend$segments,aes(x = y, y = x, xend = yend, yend = xend)) +
          scale_x_reverse() +
          scale_y_continuous(breaks = seq_along(dend$labels$label), 
                             labels = dend$labels$label,position = 'right',
                             expand = c(0.005,0)) +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt"),
                axis.text.y = element_blank()) +
          labs(x = NULL,
               y = NULL)  
        
        plo <- dend_plot + plo + plot_layout(widths = c(1, 2))
      }
      
      return(plo)
    })
}

#' @importFrom ggplot2 scale_fill_gradient2

heatmapRegression <- function(pl, x, threshold, distanceMeasure, clusterMethod, featureNames, dendrogram){
  pl %>%
    map(~{
      r <- .
      
      pred <- r$Response[1]
      
      feat <- r$Feature %>%
        unique()
      
      p <- sym(pred)
      
      d <- x@data %>%
        keepFeatures(features = feat)
      
      d <- d %>%
        sinfo() %>%
        select(pred) %>%
        bind_cols(d %>%
                    dat()) %>%
        rowid_to_column(var = 'Sample') %>%
        gather('Feature','Intensity',-1,-2) %>%
        group_by(Feature) %>%
        summarise(r = cor(!! p,Intensity)) %>%
        mutate(Response = pred)
      
      suppressWarnings({
        dend <- d %>%
          spread(3,r) %>%
          set_rownames(.$Feature) %>%
          select(-Feature) %>%
          dist(distanceMeasure) %>%
          hclust(clusterMethod) %>%
          dendro_data()  
      })
      
      clusters <- dend$labels$label
      
      d <- d %>%
        ungroup() %>%
        mutate(Feature = factor(Feature,levels = clusters)) %>%
        mutate(Response = factor(Response))
      
      caption <- str_c('Explanatory features had a P value below a threshold of ',threshold,'.')
      
      if (length(feat) > 3000) {
        caption <- str_c(caption,'\n','Number of features capped at top 3000.')
      }
      low <- '#00B7FF'
      mid <- 'white'
      high <- "#F21A00"
      
      plo <- d %>%
        ggplot(aes_string(x = 'Response',y = 'Feature',fill = 'r')) +
        geom_tile(colour = 'black') +
        scale_fill_gradient2(low = low, mid = mid,high = high,limits=c(-1,1)) +
        scale_y_discrete(expand = c(0,0),position = 'right') +
        theme_minimal(base_size = 8) +
        labs(title = pred,
             caption = caption,
             fill = 'Relative\nIntensity')
      if (isTRUE(featureNames)) {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold'),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      } else {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold'),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                axis.text.y = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      }
      
      if (isTRUE(dendrogram)) {
        dend_plot <- ggplot() +
          geom_segment(data = dend$segments,aes(x = y, y = x, xend = yend, yend = xend)) +
          scale_x_reverse() +
          scale_y_continuous(breaks = seq_along(dend$labels$label), 
                             labels = dend$labels$label,position = 'right',
                             expand = c(0.005,0)) +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt"),
                axis.text.y = element_blank()) +
          labs(x = NULL,
               y = NULL)  
        
        plo <- dend_plot + plo + plot_layout(widths = c(1, 2))
      }
      
      return(plo)
    })
}

#' plotExplanatoryHeatmap
#' @rdname plotExplanatoryHeatmap
#' @description plot a heatmap of explanatory features
#' @param x object of class Univariate, RandomForest or Analysis containing modelling results
#' @param measure importance measure on which to retrieve explanatory feautres
#' @param threshold score threshold to use for specifying explantory features
#' @param distanceMeasure distance measure to use for clustering. See details.
#' @param clusterMethod clustering method to use. See details
#' @param featureNames should feature names be plotted?
#' @param dendrogram TRUE/FALSE. Should the dendrogram be plotted?
#' @param ... arguments to pass to the appropriate method
#' @details 
#' Options for distance measures are as for \code{dist()}.
#' Clustering methods are as given for \code{hclust()}.
#' @seealso \link{dist} \link{hclust}
#' @importFrom stats dist hclust
#' @importFrom ggdendro dendro_data 
#' @importFrom ggplot2 geom_tile scale_fill_gradient theme_minimal labs element_blank
#' @importFrom stringr str_split_fixed
#' @importFrom tibble deframe
#' @importFrom dplyr group_by_at mutate_at
#' @importFrom magrittr set_rownames
#' @importFrom rlang sym
#' @examples \dontrun{
#' 
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters(c('preTreat','featureSelection'))
#' p@preTreat <- list(
#'     remove = list(class = list(classes = 4:6)),
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p) 
#' plotExplanatoryHeatmap(analysis)
#' }
#' @export

setMethod('plotExplanatoryHeatmap',signature = 'Univariate',
          function(x, threshold = 0.05, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', featureNames = TRUE, dendrogram = TRUE){
            
            res <- x %>%
              explanatoryFeatures()
            
            pl <- res %>%
              base::split(.$Response)
            
            if (x@type == 't-test' | x@type == 'ANOVA') {
              pl <- heatmapClasses(pl,x, threshold = threshold, distanceMeasure = distanceMeasure, clusterMethod = clusterMethod, featureNames = featureNames,dendrogram = dendrogram)
            }
            
            if (x@type == 'linear regression') {
              pl <- heatmapRegression(pl,x, threshold = threshold, distanceMeasure = distanceMeasure, clusterMethod = clusterMethod, featureNames = featureNames,dendrogram = dendrogram)
            }
            
            # pl <- wrap_plots(pl)
            
            p <- pl[[1]]
            
            if (length(pl) > 1) {
              for (i in 2:length(pl)) {
                p <- p + pl[[i]]
              }  
            }
            
            return(p)
          }
)

#' @rdname plotExplanatoryHeatmap
#' @export

setMethod('plotExplanatoryHeatmap',signature = 'RandomForest',
          function(x, measure = 'FalsePositiveRate', threshold = 0.05, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', featureNames = TRUE, dendrogram = TRUE){
            
            if (x@type == 'unsupervised') {
              stop('Cannot plot heatmap for unsupervised random forest.')
            }
            
            explan <- explanatoryFeatures(x,measure = measure,threshold = threshold)
            
            if ('Response' %in% colnames(explan)) {
              pl <- explan %>%
                base::split(.$Response)
            } else {
              pl <- list(explan)
            }
            
            if (x@type == 'classification') {
              pl <- heatmapClasses(pl,x, threshold = threshold, distanceMeasure = distanceMeasure, clusterMethod = clusterMethod, featureNames = featureNames,dendrogram = dendrogram)
            }
            
            if (x@type == 'regression') {
              pl <- heatmapRegression(pl,x, threshold = threshold, distanceMeasure = distanceMeasure, clusterMethod = clusterMethod, featureNames = featureNames,dendrogram = dendrogram)
            }
            
            pl <- wrap_plots(pl)
            
            return(pl)
          }
)

#' @rdname plotExplanatoryHeatmap
#' @export

setMethod('plotExplanatoryHeatmap',signature = 'list',
          function(x, threshold = 0.05, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', featureNames = T){
            object_classes <- x %>%
              map_chr(class)
            
            if (F %in% (object_classes == 'RandomForest' | object_classes == 'Univariate')) {
              stop('All objects contained within supplied list should be of class RandomForest or Univariate',call. = FALSE)
            }
            
            x %>%
              map(plotExplanatoryHeatmap,threshold = threshold, distanceMeasure = distanceMeasure, clusterMethod = clusterMethod, featureNames = featureNames)
          }
)

#' @rdname plotExplanatoryHeatmap
#' @export

setMethod('plotExplanatoryHeatmap',signature = 'Analysis',
          function(x, threshold = 0.05, distanceMeasure = "euclidean", clusterMethod = 'ward.D2', featureNames = T){
            x %>%
              modellingResults() %>%
              map(~{
                map(.,plotExplanatoryHeatmap,threshold = threshold, distanceMeasure = distanceMeasure, clusterMethod = clusterMethod, featureNames = featureNames) %>%
                  wrap_plots()  
              })
          }
)
