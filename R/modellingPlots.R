#' plotImportance
#' @rdname plotImportance
#' @description Plot univariate or random forest feature importance.
#' @param x S4 object of class Univariate or RandomForest
#' @param response Response results to plot
#' @param ... arguments to pass to specific method
#' @importFrom ggplot2 facet_wrap
#' @export

setMethod('plotImportance',signature = 'Univariate',
          function(x, response = 'class'){
            threshold <- 0.05
            res <- x@results
            
            if (!(response %in% unique(res$Response))) {
              stop('Response not found!')
            }
            
            res <- res %>%
              filter(Response == response) %>%
              mutate(`-log10(p)` = -log10(adjusted.p.value))
            
            pl <- ggplot(res,aes(x = Feature,y = `-log10(p)`)) +
              geom_hline(yintercept = -log10(threshold),linetype = 2,colour = 'red') +
              geom_point(shape = 21,alpha = 0.5,fill = ptol_pal()(1)) +
              theme_bw() +
              theme(axis.line.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.text.x = element_blank(),
                    panel.grid = element_blank(),
                    axis.title = element_text(face = 'bold'),
                    plot.title = element_text(face = 'bold')) +
              labs(title = response,
                   caption = str_c('Dashed red line shows threshold of ',threshold,'.')) 
            
            if (x@type == 'ttest') {
              pl <- pl +
                facet_wrap(~Comparison)
            }
            
            return(pl)
          }
)

#' plotMeasures
#' @rdname plotMeasures
#' @description Plot random forest model measures.
#' @param x S4 object of class RandomForest
#' @param response response results to plot
#' @importFrom ggplot2 xlim
#' @export

setMethod('plotMeasures',signature = 'RandomForest',
          function(x){
            
            if (x@type == 'Unsupervised') {
              stop('No measures to plot for unsupervised random forest.')
            }
            
            res <- measures(x)
            
            response <- res$Response %>%
              unique()
            
            if (x@type == 'classification') {
              pl <- ggplot(res,aes(x = .estimate,y = Comparison)) +
                geom_point(shape = 21,fill = ptol_pal()(1)) +
                theme_bw() +
                facet_wrap(~.metric) +
                labs(title = response,
                     x = '') +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold')) +
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
                      axis.title = element_text(face = 'bold'))
            }
            
            
            return(pl)
          }
)

#' plotImportance
#' @rdname plotImportance
#' @export

setMethod('plotImportance',signature = 'RandomForest',
          function(x){
            
            res <- importance(x)
            response <- res$Response[1]
            
            if ('adjustedPvalue' %in% colnames(res)) {
              res <- res %>%
                mutate(`-log10(p)` = -log10(adjustedPvalue))
              
              if (x@type == 'classfication') {
                res <- res %>%
                  filter(Measure == 'FalsePositiveRate')
              }
              
              metric <- '`-log10(p)`'
            } else {
              if (x@type == 'unsupervised' | x@type == 'classification') {
                res <- res %>%
                  filter(Measure == 'FalsePositiveRate') %>%
                  mutate(`-log10(fpr)` = -log10(Value))
                metric <- '`-log10(fpr)`'
              }
              
              if (x@type == 'regression') {
                metric <- 'IncNodePurity'
                res <- res %>%
                  filter(Measure == 'IncNodePurity') %>%
                  spread(Measure,Value)
              }
              
            }
            
            
            
            pl <- ggplot(res,aes_string(x = 'Feature',y =
                                          metric)) +
              geom_point(shape = 21,alpha = 0.5,fill = ptol_pal()(1)) +
              theme_bw() +
              theme(axis.line.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.text.x = element_blank(),
                    panel.grid = element_blank(),
                    axis.title = element_text(face = 'bold'),
                    plot.title = element_text(face = 'bold'))
            
            if (x@type != 'unsupervised') {
              pl <- pl +
                labs(title = response)
            }
            
            if (x@type == 'classification') {
              pl <- pl +
                facet_wrap(~Comparison)
            }
            
            return(pl)
          }
)

#' plotMDS
#' @rdname plotMDS
#' @description Plot multidimensional scaling plot for a RandomForest object.
#' @param x S4 object of class RandomForest
#' @param cls info column to use for sample labelling, Set to NULL for no labelling. 
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param title plot title
#' @param legend TRUE/FALSE should a legend be plotted. Useful for many classes. Defaults to TRUE. 
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}. Ignored if \code{legend = FALSE}.
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importFrom magrittr set_colnames
#' @importFrom dplyr mutate_all
#' @importFrom tidyr spread
#' @importFrom ggthemes scale_colour_ptol scale_fill_ptol ptol_pal
#' @importFrom ggrepel geom_text_repel
#' @export

setMethod('plotMDS',signature = 'RandomForest',
          function(x,cls = 'class', label = NULL, ellipses = T, title = '', legend = TRUE, legendPosition = 'bottom', labelSize = 2){
            
            if (!(cls %in% {x@data %>% sinfo() %>% colnames()})) {
              stop(str_c('Info column ',cls,'not found!'))
            }
            
            if (x@type == 'classification') {
              proximities <- x@proximities %>%
                base::split(.$Comparison) %>%
                map(~{
                  d <- .
                  d %>%
                    group_by(Sample1,Sample2) %>%
                    summarise(Proximity = mean(Proximity)) %>%
                    spread(Sample2,Proximity) %>%
                    tbl_df() %>%
                    select(-Sample1)
                }) 
              suppressWarnings({
                mds <- proximities %>%
                  map(~{
                    d <- .
                    d %>%
                      {1 - .} %>%
                      cmdscale() %>%
                      as_tibble() %>%
                      set_colnames(c('Dimension 1','Dimension 2')) 
                  }) %>%
                  bind_rows(.id = 'Comparison')
              })
              
              if (!is.null(cls)) {
                mds <- mds %>%
                  base::split(.$Comparison) %>%
                  map(~{
                    d <- .
                    
                    comparison <- str_split(d$Comparison[1],'~')[[1]]
                    
                    cda <- removeClasses(x@data,cls,classes = sinfo(x@data) %>%
                                           select(cls) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% comparison)])
                    
                    d %>%
                      bind_cols(cda %>%
                                  sinfo() %>%
                                  select(cls) %>%
                                  mutate_all(as.character)
                      )  
                  }) %>%
                  bind_rows()
                
              }
              
              if (!is.null(label)) {
                mds <- mds %>%
                  base::split(.$Comparison) %>%
                  map(~{
                    d <- .
                    comparison <- str_split(d$Comparison[1],'~')[[1]]
                    
                    cda <- removeClasses(x@data,cls,classes = sinfo(x@data) %>%
                                           select(cls) %>%
                                           unlist() %>%
                                           unique() %>%
                                           .[!(. %in% comparison)])
                    
                    d %>%
                      bind_cols(cda %>%
                                  sinfo() %>%
                                  select(label) %>%
                                  mutate_all(as.character)
                      )  
                  }) %>%
                  bind_rows()
              }
              
            } else {
              proximities <- x@proximities %>%
                group_by(Sample1,Sample2) %>%
                summarise(Proximity = mean(Proximity)) %>%
                spread(Sample2,Proximity) %>%
                tbl_df() %>%
                select(-Sample1)
              
              suppressWarnings({
                mds <- proximities %>%
                  {1 - .} %>%
                  cmdscale() %>%
                  as_tibble() %>%
                  set_colnames(c('Dimension 1','Dimension 2'))
              })  
              if (!is.null(cls)) {
                mds <- mds %>%
                  bind_cols(x@data %>%
                              sinfo() %>%
                              select(cls) %>%
                              mutate_all(factor)
                  )
              }
              
              if (!is.null(label)) {
                mds <- mds %>%
                  bind_cols(x@data %>%
                              sinfo() %>%
                              select(label))
              }
            }
            
            pl <- ggplot(mds,aes(x = `Dimension 1`,y = `Dimension 2`)) +
              coord_fixed() +
              theme_bw()
            
            if (legend == TRUE) {
              pl <- pl +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold'),
                      legend.title = element_text(face = 'bold'),
                      legend.position = legendPosition
                )
            } else {
              pl <- pl +
                theme(
                  plot.title = element_text(face = 'bold'),
                  axis.title = element_text(face = 'bold'),
                  legend.position = 'none'
                )
            }
              
            
            if (isTRUE(ellipses) & !(is.null(cls))) {
              pl <- pl +
                stat_ellipse(aes_string(fill = cls),alpha = 0.3,geom = 'polygon',type = 'norm')
            }
            
            if (!is.null(cls)) {
              classLength <- mds[,cls] %>%
                unlist(use.names = F) %>%
                unique() %>%
                length()
              
              if (classLength <= 12) {
                pl <- pl + 
                  scale_colour_ptol() +
                  scale_fill_ptol()
              } else {
                if (classLength %% 12 == 0) {
                  pal <- rep(ptol_pal()(12),classLength / 12)
                } else {
                  pal <- c(rep(ptol_pal()(12),floor(classLength / 12)),ptol_pal()(12)[1:(classLength %% 12)])
                }
                pl <- pl + 
                  scale_colour_manual(values = pal) +
                  scale_fill_manual(values = pal)
              }
              
              if (classLength > 6) {
                sym <- 0:25
                if (classLength / max(sym) == 1) {
                  val <- sym
                }
                if (classLength / max(sym) < 1) {
                  val <- sym[1:classLength]
                }
                if (classLength / max(sym) > 1) {
                  if (classLength %% max(sym) == 0) {
                    val <- rep(sym,classLength / max(sym))
                  } else {
                    val <- c(rep(sym,floor(classLength / max(sym))),sym[1:(classLength %% max(sym))])
                  }
                }
                pl <- pl + scale_shape_manual(values = val)
              }
              
              pl <- pl +
                geom_point(aes_string(colour = cls,shape = cls))
            } else {
              pl <- pl +
                geom_point(shape = 21,fill = ptol_pal()(1))
            }
            
            if (!is.null(label)) {
              pl <- pl +
                geom_text_repel(aes_string(label = label),size = labelSize)
            }
            
            pl <- pl +
              labs(title = title)
            
            if (x@type == 'classification') {
              pl <- pl +
                facet_wrap(~Comparison)
            }
            
            return(pl)
          }
)

#' plotROC
#' @rdname plotROC
#' @description plot reciever operator characteristic curves for a RandomForest object.
#' @param x S4 object of class RandomForest
#' @param title plot title
#' @param legend TRUE/FALSE should a legend be plotted. Useful for many classes. Defaults to TRUE. 
#' @importFrom ggplot2 geom_abline geom_line guide_legend
#' @importFrom yardstick roc_curve
#' @export

setMethod('plotROC',signature = 'RandomForest',
          function(x,title = '',legend = TRUE){
            
            if (x@type != 'classification') {
              stop('ROC curves can only be plotted for classification!')
            }
            
            preds <- x@predictions %>%
              base::split(.$Comparison) %>%
              map(~{
                d <- .
                d <- d %>%
                  mutate(obs = factor(obs))
                
                suppressMessages({
                  suppressWarnings({
                    if (length(levels(d$obs)) > 2) {
                      d %>%
                        group_by(Comparison) %>%
                        roc_curve(obs,levels(d$obs))  
                    } else {
                      d %>%
                        group_by(Comparison) %>%
                        roc_curve(obs,levels(d$obs)[1])  
                    }  
                  })   
                })
              }) %>%
              bind_rows()
           
            meas <- x@results$measures %>%
              filter(.metric == 'roc_auc') %>%
              mutate(x = 0.8,y = 0, label = str_c('AUC: ',round(.estimate,3)))
             
            if ('.level' %in% colnames(preds)) {
              preds <- preds %>%
                arrange(.level,sensitivity)
              
              pl <- preds %>%
                ggplot() +
                geom_abline(intercept = 0,linetype = 2,colour = 'grey') +
                geom_line(aes(x = 1 - specificity, y = sensitivity,group = .level,colour = .level)) +
                geom_text(data = meas,aes(x = x,y = y,label = label),size = 3) +
                theme_bw() +
                facet_wrap(~Comparison) +
                coord_fixed() +
                guides(colour = guide_legend(title = x@results$measures$Response[1])) +
                labs(title = title)
              
              if ((preds$.level %>% unique() %>% length()) <= 12) {
                pl <- pl +
                  scale_colour_ptol()
              }  
            } else {
              
                preds <- preds %>%
                  arrange(sensitivity)
                
              pl <- preds %>%
                ggplot() +
                geom_abline(intercept = 0,linetype = 2,colour = 'grey') +
                geom_line(aes(x = 1 - specificity, y = sensitivity),colour = ptol_pal()(1)) +
                geom_text(data = meas,aes(x = x,y = y,label = label),size = 3) +
                theme_bw() +
                facet_wrap(~Comparison) +
                coord_fixed() +
                guides(colour = guide_legend(title = 'Class')) +
                labs(title = title)
            }
            
            if (legend == TRUE) {
              pl <- pl +
                theme(legend.position = 'bottom',
                      axis.title = element_text(face = 'bold'),
                      legend.title = element_text(face = 'bold'))
            } else {
              pl <- pl +
                theme(legend.position = 'none',
                      axis.title = element_text(face = 'bold'))
            }
            
            return(pl)
          }
)