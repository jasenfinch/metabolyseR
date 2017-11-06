#' @importFrom ggplot2 scale_shape_manual
#' @importFrom stringr str_c
#' @export

setMethod('plotPCA',signature = 'Analysis',
          function(analysis, cls = 'class', scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2'){
            dat <- preTreatedData(analysis)
            info <- dat$Info %>%
              select(cls)
            dat <- dat$Data
            colnames(info)[1] <- 'Class'
            
            pca <- prcomp(dat,scale. = scale,center = center)
            var <- pca$sdev
            var <- round(var^2/sum(var^2) * 100,2)
            names(var) <- colnames(pca$x)
            
            pca <- pca$x %>%
              as_tibble() %>%
              select(xAxis = xAxis,yAxis = yAxis) %>%
              bind_cols(info)
            
            pl <- pca %>%
              ggplot(aes(x = xAxis,y  = yAxis,colour = Class,shape = Class)) +
              geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
              geom_vline(xintercept = 0,linetype = 2,colour = 'grey') +
              geom_point() +
              theme_bw() +
              xlab(str_c(xAxis,' (Var: ',var[xAxis],'%)')) +
              ylab(str_c(yAxis,' (Var: ',var[yAxis],'%)'))
            
            classLength <- info %>%
              unique() %>%
              nrow()
            
            if (classLength <= 12) {
              pl <- pl + scale_colour_ptol()
            } else {
              if (classLength %% 12 == 0) {
                pal <- rep(ptol_pal()(12),classLength / 12)
              } else {
                pal <- c(rep(ptol_pal()(12),floor(classLength / 12)),ptol_pal()(12)[1:(classLength %% 12)])
              }
              pl <- pl + scale_colour_manual(values = pal)
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
            
            pl
          }
)