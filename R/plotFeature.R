#' Plot a feature
#' @rdname plotFeature
#' @description Plot the trend of a feature.
#' @param analysis an object of class `AnalysisData` or`` Analysis`
#' @param feature feature name to plot
#' @param cls information column to use for class labels
#' @param label information column to use for sample labels
#' @param labelSize sample label size
#' @param type `raw` or `pre-treated` data to plot
#' @param ... arguments to pass to the appropriate method
#' @examples 
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg,abr1$fact)
#' 
#' ## Plot a categorical response variable
#' plotFeature(d,'N133',cls = 'day')
#' 
#' ## Plot a continuous response variable
#' plotFeature(d,'N133',cls = 'injorder')
#' @export

setGeneric('plotFeature',
           function(
             analysis, 
             feature,
             cls = 'class', 
             label = NULL, 
             labelSize = 2, 
             ...)
             standardGeneric('plotFeature'))

#' @rdname plotFeature
#' @importFrom ggplot2 aes geom_point theme_bw element_text guides 
#' @importFrom ggplot2 scale_fill_manual xlab

setMethod('plotFeature',
          signature = 'AnalysisData',
          function(analysis, 
                   feature, 
                   cls = 'class', 
                   label = NULL, 
                   labelSize = 2){
            
            feat <- features(analysis)
            
            if (!feature %in% feat) {
              stop(
                str_c('Feature "',
                      feature,
                      '" is not present in this dataset'),
                call. = FALSE)
            }
            
            d <- dat(analysis)
            i <- sinfo(analysis)
            
            i <- i %>%
              select(Class = cls,Label = label)
            
            if (!is.null(label)) {
              d <- d %>%
                bind_cols(i) %>%
                gather('Feature','Intensity',-Class,-Label) %>%
                filter(Feature == feature) %>%
                mutate(Intensity = as.numeric(Intensity))
            } else {
              d <- d %>%
                bind_cols(i) %>%
                gather('Feature','Intensity',-Class) %>%
                filter(Feature == feature) %>%
                mutate(Intensity = as.numeric(Intensity))
            }
            
            if (class(i$Class) == 'character' | class(i$Class) == 'factor') {
              classes <- d %>%
                select(Class) %>% 
                unique() %>%
                unlist() %>%
                length()
              
              pl <- d %>%
                ggplot(aes(x = Class,y = Intensity,group = Class)) +
                geom_boxplot(outlier.shape = NA,colour = 'darkgrey') +
                geom_point(aes(fill = Class),shape = 21,alpha = 0.8) +
                theme_bw() +
                ggtitle(feature) +
                theme(axis.title = element_text(face = 'bold'),
                      plot.title = element_text(face = 'bold',
                                                hjust = 0.5),
                      panel.grid = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_line()) +
                guides(fill = FALSE)
              
              if (classes <= 12) {
                pl <- pl + scale_fill_ptol()
              } else {
                if (classes %% 12 == 0) {
                  pal <- rep(ptol_pal()(12),classes / 12)
                } else {
                  pal <- c(
                    rep(ptol_pal()(12),floor(classes / 12)),
                    ptol_pal()(12)[1:(classes %% 12)])
                }
                pl <- pl + scale_fill_manual(values = pal)
              }
            } else {
              pl <- d %>%
                ggplot(aes(x = Class, y = Intensity)) +
                geom_point(fill = ptol_pal()(1),shape = 21) +
                theme_bw() +
                ggtitle(feature) +
                xlab(cls) +
                theme(axis.title = element_text(face = 'bold'),
                      plot.title = element_text(face = 'bold',
                                                hjust = 0.5),
                      panel.grid = element_blank(),
                      panel.border = element_blank(),
                      axis.line = element_line())
            }
            
            if (!is.null(label)) {
              pl <- pl +
                geom_text_repel(aes(label = Label),size = labelSize)
            }
            
            pl
          }
)

#' @rdname plotFeature
#' @importFrom ggplot2 ggtitle

setMethod('plotFeature',
          signature = 'Analysis',
          function(analysis, 
                   feature, 
                   cls = 'class', 
                   label = NULL, 
                   labelSize = 2, 
                   type = 'pre-treated'){
            if (!(type %in% c('raw','pre-treated'))) {
              stop(
                'Argument "type" should be one of "raw" or "pre-treated".',
                call. = FALSE)
            }
            
            if (type == 'pre-treated') {
              d <- analysis %>%
                preTreated()
            } else {
              d <- analysis %>%
                raw()
            }
            
            d %>%
              plotFeature(feature = feature,
                          cls = cls,
                          label = label,
                          labelSize = labelSize)
          })
