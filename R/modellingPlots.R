#' Plot feature importance
#' @rdname plotImportance
#' @description Plot Univariate or random forest feature importance.
#' @param x S4 object of class `Univariate` or `RandomForest`
#' @param response response results to plot
#' @param metric importance metric to plot
#' @param rank rank feature order for plotting
#' @param threshold explanatory threshold line for the output plot
#' @param ... arguments to pass to specific method
#' @examples 
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        keepClasses(cls = 'day',classes = c('H','1','5')) %>% 
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' plotImportance(rf,rank = FALSE)
#' @export

setGeneric("plotImportance", function(x,...)
  standardGeneric("plotImportance"))

#' @rdname plotImportance
#' @importFrom ggplot2 facet_wrap

setMethod('plotImportance',signature = 'Univariate',
          function(x, response = 'class',rank = TRUE,threshold = 0.05){
            
            res <- importance(x)
            
            available_responses <- res$Response %>%
              unique()
            
            if (!(response %in% unique(res$Response))) {
              ar <- available_responses %>%
                str_c('"',.,'"') %>%
                str_c(collapse = ', ')
              
              if (length(available_responses) > 1) {
                stop(
                  str_c('Response "',response,'" not found! Responses ',
                        ar,' are available for this Univariate class object.'),
                  call. = FALSE) 
              } else {
                stop(
                  str_c('Response "',response,'" not found! Response ',
                        ar,' is available for this Univariate class object.'),
                  call. = FALSE)
              }
            }
            
            res <- res %>%
              filter(Response == response) %>%
              mutate(`-log10(p)` = -log10(adjusted.p.value))
            
            pl <- res %>%
              base::split(.$Comparison) %>%
              map(~{
                if (isTRUE(rank)) {
                  .x <- .x %>%
                    arrange(`-log10(p)`)
                  
                  rank <- .x$Feature
                  
                  .x <- .x %>%
                    mutate(Feature = factor(Feature,levels = rank))
                }
                
                comparison <- .x$Comparison[1]
                
                ggplot(.x,aes(x = Feature,y = `-log10(p)`)) +
                  geom_hline(
                    yintercept = -log10(threshold),
                    linetype = 2,
                    colour = 'red') +
                  geom_point(shape = 21,alpha = 0.5,fill = ptol_pal()(1)) +
                  theme_bw() +
                  theme(axis.ticks.x = element_blank(),
                        axis.text.x = element_blank(),
                        panel.grid = element_blank(),
                        panel.border = element_blank(),
                        axis.line = element_line(),
                        axis.title = element_text(face = 'bold'),
                        plot.title = element_text(face = 'bold',
                                                  hjust = 0.5)) +
                  labs(title = comparison)
                
              }) %>%
              wrap_plots() +
              plot_annotation(title = response,
                              caption = str_c(
                                'Dashed red line shows threshold of ',
                                threshold,'.'),
                              theme = theme(plot.title = element_text(face = 'bold',
                                                                      hjust = 0.5),
                                            plot.caption = element_text(hjust = 0)))
            
            return(pl)
          }
)

#' @rdname plotImportance
#' @importFrom stringr str_to_title

setMethod('plotImportance',signature = 'RandomForest',
          function(x,metric = 'false_positive_rate',rank = TRUE){
            
            typ <- type(x)
            metrics <- importanceMetrics(x)
            current_metric <- metric
            
            if (!(metric %in% metrics)) {
              
              metrics <- str_c('"',metrics,'"')
              
              stop(
                'Argument "metric" should be one of ',
                str_c(metrics,collapse = ', '),
                call. = FALSE)
            }
            
            res <- importance(x) %>%
              filter(metric == current_metric)
            
            if (typ == 'classification') {
              pl <- res %>%
                base::split(.$comparison) %>%
                map(~{
                  if (isTRUE(rank)) {
                    .x <- .x %>%
                      arrange(value)
                    
                    rank <- .x$feature
                    
                    .x <- .x %>%
                      mutate(feature = factor(feature,levels = rank))
                  }
                  
                  .x <- .x %>%
                    spread(metric,value)
                  
                  comparison <- .x$comparison[1]
                  
                  pl <- ggplot(.x,aes(x = feature,y = !!sym(metric))) +
                    geom_point(shape = 21,alpha = 0.5,fill = ptol_pal()(1)) +
                    theme_bw() +
                    theme(axis.ticks.x = element_blank(),
                          axis.text.x = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line(),
                          axis.title = element_text(face = 'bold'),
                          plot.title = element_text(face = 'bold',
                                                    hjust = 0.5),
                          plot.caption = element_text(hjust = -1)) +
                    labs(title = comparison,
                         x = 'Feature',
                         y = str_replace_all(metric,'_',' ') %>% 
                           str_to_title())
                  
                  if (typ != 'unsupervised') {
                    pl <- pl +
                      labs(title = res$response[1],
                           x = 'Feature',
                           y = str_replace_all(metric,'_',' ') %>% 
                             str_to_title())
                  }
                  
                }) %>%
                wrap_plots()
            } else {
              pl <- res %>%
                {
                  d <- .
                  if (isTRUE(rank)) {
                    d <- d %>%
                      arrange(value)
                    
                    rank <- d$feature
                    
                    d <- d %>%
                      mutate(feature = factor(feature,levels = rank))
                  }
                  d
                } %>%
                spread(metric,value) %>%
                {
                  p <- ggplot(.,aes(x = feature,y = !!sym(metric))) +
                    geom_point(shape = 21,alpha = 0.5,fill = ptol_pal()(1)) +
                    theme_bw() +
                    theme(axis.ticks.x = element_blank(),
                          axis.text.x = element_blank(),
                          panel.grid = element_blank(),
                          panel.border = element_blank(),
                          axis.line = element_line(),
                          axis.title = element_text(face = 'bold'),
                          plot.title = element_text(face = 'bold',
                                                    hjust = 0.5),
                          plot.caption = element_text(hjust = -1))  
                  
                  if (typ != 'unsupervised') {
                    p <- p +
                      labs(title = res$response[1],
                           x = 'Feature',
                           y = str_replace_all(metric,'_',' ') %>% 
                             str_to_title())
                  }
                  p
                }
            }
            
            
            return(pl)
          }
)

#' @rdname plotImportance
#' @export

setMethod('plotImportance',
          signature = 'list',
          function(x,metric = 'false_positive_rate'){
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
              map(plotImportance,metric = metric)
          })

#' Plot model performance metrics
#' @rdname plotMetrics
#' @description Plot random forest model performance metrics
#' @param x S4 object of class `RandomForest`
#' @param response response results to plot
#' @examples 
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        keepClasses(cls = 'day',classes = c('H','1','5')) %>% 
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day',binary = TRUE)
#' 
#' plotMetrics(rf,response = 'day')
#' @export

setGeneric("plotMetrics", function(x, response = 'class')
  standardGeneric("plotMetrics"))

#' @rdname plotMetrics
#' @importFrom ggplot2 xlim

setMethod('plotMetrics',signature = 'RandomForest',
          function(x){
            
            if (x@type == 'unsupervised') {
              stop('No metrics to plot for unsupervised random forest.',
                   call. = FALSE)
            }
            
            res <- metrics(x)
            
            response <- response(x)
            
            if (x@type == 'classification') {
              pl <- ggplot(res,aes(x = .estimate,y = Comparison)) +
                geom_point(shape = 21,fill = ptol_pal()(1)) +
                theme_bw() +
                facet_wrap(~.metric) +
                labs(title = response,
                     x = '') +
                theme(plot.title = element_text(face = 'bold',
                                                hjust = 0.5),
                      axis.title = element_text(face = 'bold'),
                      panel.border = element_blank(),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      axis.line = element_line(),
                      strip.background = element_blank(),
                      strip.text = element_text(face = 'bold')) +
                xlim(c(min(res$.estimate),1))  
            }
            
            if (x@type == 'regression') {
              pl <- ggplot(res,aes(x = .estimate,y = .metric)) +
                geom_point(shape = 21,fill = ptol_pal()(1)) +
                theme_bw() +
                labs(title = response,
                     x = '',
                     y = 'Metric') +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold'),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_line(),
                      strip.background = element_blank(),
                      strip.text = element_text(face = 'bold'))
            }
            
            
            return(pl)
          }
)

#' @rdname plotMetrics

setMethod('plotMetrics',signature = 'list',function(x){
  object_classes <- x %>%
    map_chr(class)
  
  if (FALSE %in% (object_classes == 'RandomForest')) {
    stop(
      str_c('All objects contained within supplied list',
            ' should be of class RandomForest or Univariate'),
      call. = FALSE)
  }
  
  x %>%
    map(plotMetrics)
})

#' Multidimensional scaling (MDS) plot
#' @rdname plotMDS
#' @description Plot multidimensional scaling plot for a `RandomForest` class object.
#' @param x S4 object of class `RandomForest`
#' @param cls sample information column to use for sample labelling, 
#' Set to NULL for no labelling. 
#' @param label sample information column to use for sample labels. Set to NULL for no labels.
#' @param shape TRUE/FALSE use shape aesthetic for plot points. 
#' Defaults to TRUE when the number of classes is greater than 12
#' @param ellipses TRUE/FALSE, plot multivariate normal distribution 95%
#' confidence ellipses for each class
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument 
#' of `ggplot2::theme`. Set to "none" to remove legend.
#' @param labelSize label size. Ignored if `label` is `NULL`
#' @examples 
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' plotMDS(rf,cls = 'day')
#' @export

setGeneric("plotMDS", 
           function(
             x,
             cls = 'class', 
             label = NULL, 
             shape = FALSE, 
             ellipses = TRUE, 
             title = '', 
             legendPosition = 'bottom', 
             labelSize = 2) 
             standardGeneric("plotMDS"))

#' @rdname plotMDS
#' @importFrom magrittr set_colnames
#' @importFrom dplyr mutate_all
#' @importFrom tidyr spread
#' @importFrom ggthemes scale_colour_ptol scale_fill_ptol ptol_pal
#' @importFrom ggrepel geom_text_repel

setMethod('plotMDS',
          signature = 'RandomForest',
          function(x,
                   cls = 'class',
                   label = NULL, 
                   shape = FALSE, 
                   ellipses = TRUE, 
                   title = '', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            
            if (!is.null(cls)) {
              if (!(cls %in% {x %>% sinfo() %>% colnames()})) {
                stop(str_c('Info column ',cls,' not found!'))
              } 
            }
            
            mds_dimensions <- mds(x)
            
            if (type(x) == 'classification') {
              if (!is.null(cls)) {
                mds_dimensions <- mds_dimensions %>%
                  base::split(.$comparison) %>%
                  map(~{
                    comparison <- str_split(.x$comparison[1],'~')[[1]]
                    
                    cda <- keepClasses(x,response(x),comparison)
                    
                    .x %>%
                      bind_cols(cda %>%
                                  sinfo() %>%
                                  select(all_of(cls)) %>%
                                  mutate_all(as.character)
                      )  
                  }) %>%
                  bind_rows()
                
              }
              
              if (!is.null(label)) {
                mds_dimensions <- mds_dimensions %>%
                  base::split(.$comparison) %>%
                  map(~{
                    d <- .
                    comparison <- str_split(d$comparison[1],'~')[[1]]
                    
                    cda <- removeClasses(x,cls,classes = sinfo(x) %>%
                                           select(all_of(cls)) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% comparison)])
                    
                    d %>%
                      bind_cols(cda %>%
                                  sinfo() %>%
                                  select(all_of(label)) %>%
                                  mutate_all(as.character)
                      )  
                  }) %>%
                  bind_rows()
              }
              
            } else {
              if (!is.null(cls)) {
                mds_dimensions <- mds_dimensions %>%
                  bind_cols(x %>%
                              sinfo() %>%
                              select(all_of(cls)) %>%
                              mutate_all(factor)
                  )
              }
              
              if (!is.null(label)) {
                mds_dimensions <- mds_dimensions %>%
                  bind_cols(x %>%
                              sinfo() %>%
                              select(label))
              }
            }
            
            if (!is.null(cls)) {
              classLength <- clsLen(x,cls) 
            } else {
              classLength <- 1
            }
            
            pl <- scatterPlot(
              mds_dimensions,
              cls,
              'dimension 1',
              'dimension 2',
              ellipses,
              shape,
              label,
              labelSize,
              legendPosition,
              classLength,
              title,
              'Dimension 1',
              'Dimension 2')
            
            if (x@type == 'classification') {
              pl <- pl +
                facet_wrap(~comparison)
            }
            
            return(pl)
          }
)

#' @rdname plotMDS

setMethod('plotMDS',
          signature = 'list',
          function(x,
                   label = NULL,
                   shape = FALSE, 
                   ellipses = TRUE, 
                   title = '', 
                   legendPosition = 'bottom', 
                   labelSize = 2){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest')) {
              stop(
                str_c('All objects contained within supplied list',
                      ' should be of class RandomForest'),
                call. = FALSE)
            }
            
            x %>%
              names() %>%
              map(~{
                plotMDS(x[[.x]],
                        cls = .x,
                        label = label,
                        shape = shape,
                        ellipses = ellipses,
                        title = .x,
                        legendPosition = legendPosition,
                        labelSize = labelSize)
                
              }) %>%
              wrap_plots()
          }
)

#' Plot receiver operator characteristic (ROC) curves
#' @rdname plotROC
#' @description Plot receiver operator characteristic curves for a 
#' `RandomForest` class object.
#' @param x S4 object of class `RandomForest`
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position 
#' argument of `ggplot2::theme`. Set to "none" to remove legend.
#' @examples 
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' plotROC(rf)
#' @export

setGeneric("plotROC", function(x, title = '', legendPosition = 'bottom')
  standardGeneric("plotROC"))

#' @rdname plotROC
#' @importFrom ggplot2 geom_abline geom_line guide_legend
#' @importFrom yardstick roc_curve

setMethod('plotROC',signature = 'RandomForest',
          function(x,title = '', legendPosition = 'bottom'){
            
            preds <- roc(x)
            
            meas <- x@metrics %>%
              filter(.metric == 'roc_auc') %>%
              mutate(x = 0.8,
                     y = 0, 
                     label = str_c('AUC: ',round(.estimate,3)))
            
            if ('Class' %in% colnames(preds)) {
              preds <- preds %>%
                arrange(Class,sensitivity)
              
              pl <- preds %>%
                ggplot() +
                geom_abline(intercept = 0,linetype = 2,colour = 'grey') +
                geom_line(
                  aes(x = 1 - specificity, 
                      y = sensitivity,
                      group = Class,
                      colour = Class)) +
                geom_text(data = meas,aes(x = x,y = y,label = label),size = 3) +
                theme_bw() +
                facet_wrap(~comparison) +
                coord_fixed() +
                guides(
                  colour = guide_legend(
                    title = x@metrics$response[1])) +
                labs(title = title)
              
              if ((preds$Class %>% unique() %>% length()) <= 12) {
                pl <- pl +
                  scale_colour_ptol()
              }  
            } else {
              
              preds <- preds %>%
                arrange(sensitivity)
              
              pl <- preds %>%
                ggplot() +
                geom_abline(intercept = 0,linetype = 2,colour = 'grey') +
                geom_line(
                  aes(x = 1 - specificity, 
                      y = sensitivity),
                  colour = ptol_pal()(1)) +
                geom_text(
                  data = meas,
                  aes(x = x,y = y,label = label),size = 3) +
                theme_bw() +
                facet_wrap(~comparison) +
                coord_fixed() +
                guides(colour = guide_legend(title = 'Class')) +
                labs(title = title)
            }
            
            pl <- pl +
              theme(legend.position = legendPosition,
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_line(),
                    strip.background = element_blank(),
                    strip.text = element_text(face = 'bold'))
            
            return(pl)
          }
)

#' @rdname plotROC

setMethod('plotROC',
          signature = 'list',
          function(x,title = '', legendPosition = 'bottom'){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest')) {
              stop(
                str_c('All objects contained within supplied list',
                      ' should be of class RandomForest'),
                call. = FALSE)
            }
            
            x %>%
              names() %>%
              map(~{
                plotROC(x[[.x]],
                       title = title,
                       legendPosition = legendPosition)
                
              }) %>%
              wrap_plots()
          }
)
