#' plotFeature
#' @rdname plotFeature
#' @description Plot a feature trend.
#' @param analysis object of class Analysis, AnalysisData, Univariate or RandomForest containing analysis results
#' @param feature feature to plot
#' @param cls info column to use for class labels
#' @param label info column to use for sample labels
#' @param labelSize sample label size
#' @param type \code{raw} or \code{preTreated} data to plot
#' @importFrom ggplot2 ggtitle
#' @examples 
#' \dontrun{
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters(c('preTreat'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)
#' plotFeature(analysis,'N133',cls = 'day')
#' }
#' @export

setMethod('plotFeature',signature = 'Analysis',
          function(analysis, feature, cls = 'class', label = NULL, labelSize = 2, type = 'preTreated'){
            ty <- get(type)
            
            analysis %>%
              ty() %>%
              plotFeature(feature = feature,cls = cls,label = label,labelSize = labelSize)
          })

#' @rdname plotFeature
#' @importFrom ggplot2 aes geom_point theme_bw element_text guides scale_fill_manual xlab
#' @export

setMethod('plotFeature',signature = 'AnalysisData',
          function(analysis, feature, cls = 'class', label = NULL, labelSize = 2){
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
                      plot.title = element_text(face = 'bold')) +
                guides(fill = FALSE)
              
              if (classes <= 12) {
                pl <- pl + scale_fill_ptol()
              } else {
                if (classes %% 12 == 0) {
                  pal <- rep(ptol_pal()(12),classes / 12)
                } else {
                  pal <- c(rep(ptol_pal()(12),floor(classes / 12)),ptol_pal()(12)[1:(classes %% 12)])
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
                      plot.title = element_text(face = 'bold'))
            }
            
            if (!is.null(label)) {
              pl <- pl +
                geom_text_repel(aes(label = Label),size = labelSize)
            }
            
            pl
          }
)

#' @rdname plotFeature
#' @export

setMethod('plotFeature',signature = 'Univariate',
          function(analysis, feature, cls = 'class', label = NULL, labelSize = 2){
            analysis@data %>%
              plotFeature(feature = feature,cls = cls,label = label,labelSize = labelSize)
          })

#' @rdname plotFeature
#' @export

setMethod('plotFeature',signature = 'RandomForest',
          function(analysis, feature, cls = 'class', label = NULL, labelSize = 2){
            analysis@data %>%
              plotFeature(feature = feature,cls = cls,label = label,labelSize = labelSize)
          })