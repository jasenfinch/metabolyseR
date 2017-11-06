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
            var <- var^2/sum(var^2) * 100
            names(var) <- colnames(pca$x)
            pca <- pca$x %>%
              as_tibble() %>%
              select(xAxis = xAxis,yAxis = yAxis) %>%
              bind_cols(info) %>%
              ggplot(aes(x = xAxis,y  = yAxis)) +
                geom_point() +
                theme_bw()
            
          }
)