#' plotFeature
#' @rdname plotFeature
#' @description Plot a feature trend.
#' @param analysis object of class Analysis containing analysis results
#' @param feature feature to plot
#' @param cls info column to use for class labels
#' @importFrom ggplot2 ggtitle
#' @examples 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)
#' plotFeature(analysis,'N133',cls = 'day')
#' @export

setMethod('plotFeature',signature = 'Analysis',
          function(analysis, feature, cls = 'class'){
            dat <- preTreatedData(analysis)
            info <- dat$Info
            dat <- dat$Data
            
            info <- info %>%
              select(Class = cls)
            
            dat <- dat %>%
              bind_cols(info) %>%
              gather('Feature','Intensity',-Class) %>%
              filter(Feature == feature)
            
            if (class(info$Class) == 'character' | class(info$Class) == 'factor') {
              classes <- dat %>%
                select(Class) %>% 
                unique() %>%
                unlist() %>%
                length()
              
              pl <- dat %>%
                ggplot(aes(x = Class,y = Intensity,colour = Class)) +
                geom_boxplot(outlier.shape = NA,colour = 'black') +
                geom_point(position = 'jitter') +
                theme_bw() +
                ggtitle(feature)
              
              if (classes <= 12) {
                pl <- pl + scale_colour_ptol()
              } else {
                if (classes %% 12 == 0) {
                  pal <- rep(ptol_pal()(12),classes / 12)
                } else {
                  pal <- c(rep(ptol_pal()(12),floor(classes / 12)),ptol_pal()(12)[1:(classes %% 12)])
                }
                pl <- pl + scale_colour_manual(values = pal)
              }
            } else {
              pl <- dat %>%
                ggplot(aes(x = Class, y = Intensity)) +
                geom_point(colour = ptol_pal()(1)) +
                theme_bw() +
                ggtitle(feature) +
                xlab(cls)
            }
            pl
          })