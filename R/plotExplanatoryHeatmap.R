#' @importFrom ggplot2 ggplot theme scale_y_discrete scale_x_discrete
#' @importFrom ggplot2 geom_segment scale_x_reverse scale_y_continuous unit

heatmapClasses <- function(pl, 
                           x, 
                           threshold, 
                           title,
                           distanceMeasure, 
                           clusterMethod, 
                           featureNames,
                           dendrogram,
                           featureLimit){
  pl %>%
    map(~{
      r <- .x
      pred <- r$response[1]
      
      classes <- r$comparison %>%
        unique() %>%
        str_split('~') %>%
        unlist() %>%
        unique()
      
      feat <- r$feature %>%
        unique()
      
      if (length(feat) > featureLimit){
        feat <- feat[1:featureLimit]
      }
      
      d <- x %>%
        keepClasses(cls = pred,
                    classes = classes) %>%
        keepFeatures(features = feat) %>% 
        aggregateMean(cls = pred) %>% 
        transformPercent() %>% 
        {
          d <- .
          dat(d) %>% 
            bind_cols(
              sinfo(d) %>% 
                select(all_of(pred))
            )
        } %>% 
        gather(
          Feature,
          `Percent Intensity`,
          -all_of(pred)
        )
      
      suppressWarnings({
        dend <- d %>%
          spread(all_of(pred),`Percent Intensity`) %>%
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
      
      low <- 'white'
      high <- "#F21A00"
      
      plo <- d %>%
        ggplot(
          aes(x = .data[[pred]],
              y = Feature,
              fill = `Percent Intensity`)) +
        geom_tile(colour = 'black') +
        scale_fill_gradient(low = low, high = high,limits=c(0,100)) +
        scale_y_discrete(expand = c(0,0),position = 'right') +
        scale_x_discrete(expand = c(0,0)) +
        theme_minimal(base_size = 8) +
        labs(title = title,
             fill = 'Percent\nIntensity')
      if (isTRUE(featureNames)) {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold',
                                          hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      } else {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold',
                                          hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                axis.text.y = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      }
      
      if (isTRUE(dendrogram)) {
        offset <- 1 / length(feat) * 0.5
        
        dend_plot <- ggplot() +
          geom_segment(
            data = dend$segments,
            aes(x = y, y = x, xend = yend, yend = xend)) +
          scale_x_reverse(expand = c(0,0)) +
          scale_y_continuous(breaks = seq_along(dend$labels$label), 
                             labels = dend$labels$label,position = 'right',
                             expand = c(offset,offset)) +
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

heatmapRegression <- function(pl, 
                              x, 
                              threshold, 
                              title,
                              distanceMeasure, 
                              clusterMethod, 
                              featureNames, 
                              dendrogram,
                              featureLimit){
  pl %>%
    map(~{
      
      response <- .x$response[1]
      
      feat <- .x$feature %>%
        unique()
      
      if (length(feat) > featureLimit){
        feat <- feat[1:featureLimit]
      }
      
      p <- sym(response)
      
      d <- x %>%
        keepFeatures(features = feat)
      
      d <- d %>%
        sinfo() %>%
        select(all_of(response)) %>%
        bind_cols(d %>%
                    dat()) %>%
        rowid_to_column(var = 'Sample') %>%
        gather('Feature','Intensity',-1,-2) %>%
        group_by(Feature) %>%
        summarise(r = cor(!! p,Intensity),.groups = 'drop') %>%
        mutate(Response = response)
      
      dend <- d %>%
        spread(3,r) %>%
        data.frame(check.names = FALSE) %>%
        set_rownames(.$Feature) %>%
        select(-Feature) %>%
        dist(distanceMeasure) %>%
        hclust(clusterMethod) %>%
        dendro_data()  
      
      clusters <- dend$labels$label
      
      d <- d %>%
        mutate(Feature = factor(Feature,levels = clusters)) %>%
        mutate(Response = factor(Response))
      
      low <- '#00B7FF'
      mid <- 'white'
      high <- "#F21A00"
      
      plo <- d %>%
        ggplot(
          aes(
            x = Response,
            y = Feature,
            fill = r)) +
        geom_tile(colour = 'black') +
        scale_fill_gradient2(low = low, mid = mid,high = high,limits=c(-1,1)) +
        scale_y_discrete(expand = c(0,0),position = 'right') +
        theme_minimal(base_size = 8) +
        labs(title = title,
             fill = 'Relative\nIntensity')
      if (isTRUE(featureNames)) {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold',
                                          hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      } else {
        plo <- plo +
          theme(plot.title = element_text(face = 'bold',
                                          hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                legend.title = element_text(face = 'bold'),
                axis.text.x = element_text(angle = 30,hjust = 1),
                axis.text.y = element_blank(),
                panel.grid = element_blank(),
                plot.margin = unit(c(0,0,0,0), "pt")
          ) 
      }
      
      if (isTRUE(dendrogram)) {
        offset <- 1 / length(feat) * 0.5
        
        dend_plot <- ggplot() +
          geom_segment(
            data = dend$segments,
            aes(x = y, y = x, xend = yend, yend = xend)) +
          scale_x_reverse() +
          scale_y_continuous(breaks = seq_along(dend$labels$label), 
                             labels = dend$labels$label,position = 'right',
                             expand = c(offset,offset)) +
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

#' Heatmap plot of explantory features
#' @rdname plotExplanatoryHeatmap
#' @description Plot a heatmap of explanatory features.
#' @param x object of class `Univariate`, `RandomForest` or 
#' `Analysis`
#' @param metric importance metric on which to retrieve explanatory features
#' @param threshold score threshold to use for specifying explanatory features
#' @param title plot title
#' @param distanceMeasure distance measure to use for clustering. See details.
#' @param clusterMethod clustering method to use. See details
#' @param featureNames should feature names be plotted?
#' @param dendrogram TRUE/FALSE. Should the dendrogram be plotted?
#' @param featureLimit The maximum number of features to plot
#' @param ... arguments to pass to method `explanatoryFeatures()`
#' @details 
#' Distance measures can be one of any that can be used for the `method` argument of [dist()].
#'
#' Cluster methods can be one of any that can be used for the `method` argument of [hclust()].
#' @examples
#' library(metaboData)
#' x <- analysisData(data = abr1$neg[,200:300],info = abr1$fact)
#' 
#' ## random forest classification example
#' random_forest <- randomForest(x,cls = 'day')
#' 
#' plotExplanatoryHeatmap(random_forest)
#' 
#' ## random forest regression example
#' random_forest <- randomForest(x,cls = 'injorder')
#' 
#' plotExplanatoryHeatmap(random_forest,metric = '%IncMSE',threshold = 2)
#' @export

setGeneric('plotExplanatoryHeatmap',function(x, ...)
  standardGeneric('plotExplanatoryHeatmap'))

#' @rdname plotExplanatoryHeatmap
#' @importFrom stats dist hclust
#' @importFrom ggdendro dendro_data 
#' @importFrom ggplot2 geom_tile scale_fill_gradient theme_minimal 
#' @importFrom ggplot2 labs element_blank
#' @importFrom stringr str_split_fixed
#' @importFrom tibble deframe
#' @importFrom dplyr group_by_at mutate_at
#' @importFrom magrittr set_rownames
#' @importFrom rlang sym

setMethod('plotExplanatoryHeatmap',
          signature = 'Univariate',
          function(x, 
                   threshold = 0.05, 
                   title = '',
                   distanceMeasure = "euclidean", 
                   clusterMethod = 'ward.D2', 
                   featureNames = TRUE, 
                   dendrogram = TRUE,
                   featureLimit = Inf,
                   ...){
            
            res <- x %>%
              explanatoryFeatures(threshold = threshold,
                                  ...)
            
            if (nrow(res) < 1){
              message('No explanatory features found at this threshold.')
              return()
            }
            
            pl <- res %>%
              base::split(.$response)
            
            if (x@type == 't-test' | x@type == 'ANOVA') {
              pl <- heatmapClasses(
                pl,
                x, 
                threshold = threshold, 
                title = title,
                distanceMeasure = distanceMeasure, 
                clusterMethod = clusterMethod, 
                featureNames = featureNames,
                dendrogram = dendrogram,
                featureLimit = featureLimit)
            }
            
            if (x@type == 'linear regression') {
              pl <- heatmapRegression(
                pl,
                x, 
                threshold = threshold, 
                title = title,
                distanceMeasure = distanceMeasure, 
                clusterMethod = clusterMethod, 
                featureNames = featureNames,
                dendrogram = dendrogram,
                featureLimit = featureLimit)
            }
            
            feat <- res$feature %>% 
              unique()
            
            caption <- str_c(
              'Explanatory features had a P value below a threshold of ',
              threshold,'.')
            
            if (length(feat) > featureLimit) {
              caption <- str_c(
                caption,'\n',
                str_c('Number of features capped at top ',featureLimit,'.'))
            }
            
            pl <- wrap_plots(pl) + 
              plot_annotation(caption = caption,
                              theme = theme(plot.caption = element_text(hjust = 0))) +
              plot_layout(guides = 'collect')
            
            return(pl)
          }
)

#' @rdname plotExplanatoryHeatmap

setMethod('plotExplanatoryHeatmap',
          signature = 'RandomForest',
          function(x, 
                   metric = 'false_positive_rate',
                   threshold = 0.05,
                   title = '',
                   distanceMeasure = "euclidean",
                   clusterMethod = 'ward.D2',
                   featureNames = TRUE, 
                   dendrogram = TRUE,
                   featureLimit = Inf,
                   ...){
            
            if (x@type == 'unsupervised') {
              stop('Cannot plot heatmap for unsupervised random forest.')
            }
            
            explan <- explanatoryFeatures(x,
                                          metric = metric,
                                          threshold = threshold,
                                          ...)
            
            if (nrow(explan) < 1){
              message('No explanatory features found at this threshold.')
              return()
            }
            
            pl <- explan %>%
              base::split(.$response)
            
            if (x@type == 'classification') {
              pl <- heatmapClasses(
                pl,
                x,
                threshold = threshold, 
                title = title,
                distanceMeasure = distanceMeasure, 
                clusterMethod = clusterMethod, 
                featureNames = featureNames,
                dendrogram = dendrogram,
                featureLimit = featureLimit)
            }
            
            if (x@type == 'regression') {
              pl <- heatmapRegression(
                pl,
                x, 
                threshold = threshold, 
                title = title,
                distanceMeasure = distanceMeasure, 
                clusterMethod = clusterMethod, 
                featureNames = featureNames,
                dendrogram = dendrogram,
                featureLimit = featureLimit)
            }
            
            feat <- explan$feature %>% 
              unique()
            
            if (metric == 'FalsePositiveRate') {
              direction <- 'below'  
            } else {
              direction <- 'above'
            }
            
            caption <- str_c(
              'Explanatory features had an importance value ',direction, ' a threshold of ',
              threshold,'.')
            
            if (length(feat) > featureLimit) {
              caption <- str_c(
                caption,'\n',
                str_c('Number of features capped at top ',featureLimit,'.'))
            }
            
            pl <- wrap_plots(pl) + 
              plot_annotation(caption = caption,
                              theme = theme(plot.caption = element_text(hjust = 0))) +
              plot_layout(guides = 'collect')
            
            return(pl)
          }
)

#' @rdname plotExplanatoryHeatmap
#' @importFrom rlang squash

setMethod('plotExplanatoryHeatmap',
          signature = 'list',
          function(x,
                   threshold = 0.05, 
                   distanceMeasure = "euclidean",
                   clusterMethod = 'ward.D2',
                   featureNames = TRUE,
                   featureLimit = Inf){
            
            suppressWarnings(x <- squash(x))
            
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest' | 
                            object_classes == 'Univariate')) {
              stop(
                str_c('All objects contained within supplied list ',
                      'should be of class RandomForest or Univariate'),
                call. = FALSE)
            }
            
            x %>% 
              map(
                ~{
                  heat_map <- try(plotExplanatoryHeatmap(
                    .x,
                    threshold = threshold, 
                    title = response(.x),
                    distanceMeasure = distanceMeasure, 
                    clusterMethod = clusterMethod,
                    featureNames = featureNames,
                    featureLimit = featureLimit
                  ))
                  
                  if (!is(heat_map,'try-error')) {
                    return(heat_map)
                  } else {
                    warning('Errors encounted in plotting heatmap, skipping.',call. = FALSE)
                  }
                  
                }
              )
          }
)

#' @rdname plotExplanatoryHeatmap

setMethod('plotExplanatoryHeatmap',
          signature = 'Analysis',
          function(x,
                   threshold = 0.05, 
                   distanceMeasure = "euclidean", 
                   clusterMethod = 'ward.D2', 
                   featureNames = TRUE,
                   featureLimit = Inf){
            pl <- x %>%
              analysisResults(element = 'modelling') %>%
              plotExplanatoryHeatmap(threshold = threshold, 
                                     distanceMeasure = distanceMeasure, 
                                     clusterMethod = clusterMethod, 
                                     featureNames = featureNames,
                                     featureLimit = featureLimit)
            
            if (length(pl) == 1){
              pl <- pl[[1]]
            }
            
            return(pl)
          }
)
