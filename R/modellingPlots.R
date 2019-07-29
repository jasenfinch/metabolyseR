#' plotResults
#' @rdname plotResults
#' @description Plot univariate modelling results.
#' @param x S4 object of class Univariate
#' @param predictor Predictor results to plot
#' @export

setMethod('plotResults',signature = 'Univariate',
          function(x, predictor = 'class'){
            threshold <- 0.05
            res <- x@results %>%
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
#' @param predictor Predictor results to plot
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
            
            
            pl <- ggplot(res,aes(x = .estimate,y = Comparison)) +
              geom_point(shape = 21,fill = ptol_pal()(1)) +
              theme_bw() +
              facet_wrap(~.metric) +
              labs(title = predictor,
                  x = '') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold')) +
              xlim(c(min(res$.estimate),1))
            
            return(pl)
          }
)
