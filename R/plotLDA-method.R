#' plotLDA
#' @rdname plotLDA
#' @description Plot linear discriminant analysis resultus of pre-treated data
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for sample labelling
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @importFrom FIEmspro nlda
#' @examples 
#' 
#' library(FIEmspro)
#' data(abr1)
#' p <- analysisParameters(c('preTreat'))
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)
#' plotLDA(analysis,cls = 'day')
#' @export

setMethod('plotLDA',signature = 'Analysis',
          function(analysis, cls = 'class', scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2'){
            dat <- preTreatedData(analysis)
            info <- dat$Info %>%
              select(cls)
            dat <- dat$Data
            colnames(info)[1] <- 'Class'
            
            classLength <- info %>%
              unique() %>%
              nrow()
            
            lda <- nlda(dat,cl = info$Class,scale = scale,center = center)
            tw <- lda$Tw %>%
              round(2)
            
            lda <- lda$x %>%
              as_tibble() %>%
              bind_cols(info) %>%
              mutate(Class = factor(Class))
            
            if (classLength > 2) {
              lda <- lda %>%
                select(Class,xAxis = xAxis,yAxis = yAxis)
              
              pl <- lda %>%
                ggplot(aes(x = xAxis,y  = yAxis,colour = Class,shape = Class)) +
                geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
                geom_vline(xintercept = 0,linetype = 2,colour = 'grey') +
                geom_point() +
                theme_bw() +
                xlab(str_c(xAxis,' (Tw: ',tw[xAxis],')')) +
                ylab(str_c(yAxis,' (Tw: ',tw[yAxis],')'))
              
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
            } else {
              pl <- lda %>%
                ggplot(aes(x = Class,y = DF1,colour = Class,shape = Class)) +
                geom_hline(yintercept = 0,linetype = 2,colour = 'grey') +
                geom_point() +
                scale_colour_ptol() +
                theme_bw() +
                guides(colour = F,shape = F) +
                ylab(str_c('DF1',' (Tw: ',tw['DF1'],')'))
            }
            
            pl
          }
) 