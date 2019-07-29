#' plotImportance
#' @rdname plotImportance
#' @description Plot univariate or random forest feature importance.
#' @param x S4 object of class Univariate or RandomForest
#' @param predictor Predictor results to plot
#' @export

setMethod('plotImportance',signature = 'Univariate',
          function(x, predictor = 'class'){
            threshold <- 0.05
            res <- x@results
            
            if (!(predictor %in% unique(res$Predictor))) {
              stop('Predictor not found!')
            }
            
            res <- res %>%
              filter(Predictor == predictor) %>%
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
              labs(title = predictor,
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
#' @param predictor predictor results to plot
#' @export

setMethod('plotMeasures',signature = 'RandomForest',
          function(x, predictor = 'class'){
            
            if (x@type == 'Unsupervised') {
              stop('No measures to plot for unsupervised random forest.')
            }
            
            res <- x@results$measures
            
            if (!(predictor %in% unique(res$Predictor))) {
              stop('Predictor not found!')
            }

            if (x@type == 'classification') {
              pl <- ggplot(res,aes(x = .estimate,y = Comparison)) +
                geom_point(shape = 21,fill = ptol_pal()(1)) +
                theme_bw() +
                facet_wrap(~.metric) +
                labs(title = predictor,
                     x = '') +
                theme(plot.title = element_text(face = 'bold'),
                      axis.title = element_text(face = 'bold')) +
                xlim(c(min(res$.estimate),1))  
            }
            
            if (x@type == 'regression') {
              pl <- ggplot(res,aes(x = .estimate,y = .metric)) +
                geom_point(shape = 21,fill = ptol_pal()(1)) +
                theme_bw() +
                labs(title = predictor,
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
            
            res <- x@results$importances
            
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
               labs(title = predictor)
            }
            
            if (x@type == 'classification') {
              pl <- pl +
                facet_wrap(~Comparison)
            }
            
            return(pl)
          }
)
