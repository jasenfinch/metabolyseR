#' plotSupervisedRF
#' @rdname plotSupervisedRF
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for sample classes
#' @param label info column to use for sample labels, Set to NULL for no labels.
#' @param seed random number seed
#' @param ... additional parameters to pass to randomForest
#' @importFrom ggrepel geom_text_repel
#' @export

setMethod('plotSupervisedRF', signature = 'Analysis',
          function(analysis, cls = 'class', label = 'sampleName', seed = 1234, ...){
            analysisPlot <- new('AnalysisPlot')
            
            analysisPlot@func <- function(analysisPlot){
              prox <- analysisPlot@data$RFresults$proximity
              
              distance <- {1 - rf$proximity} %>% 
                cmdscale() %>%
                as_tibble() %>%
                rename(`Dimension 1` = V1,`Dimension 2` = V2) %>%
                mutate(Class = analysisPlot@data$RFresults$y)
              
              if (!is.null(label)) {
                distance <- distance %>%
                  mutate(Label = analysisPlot@data$Info[,analysisPlot@data$label] %>% unlist())
              }
              
              pl <- distance %>%
                ggplot(aes(x = `Dimension 1`,y = `Dimension 2`)) +
                geom_hline(yintercept = 0,colour = 'lightgray',linetype = 2) +
                geom_vline(xintercept = 0,colour = 'lightgray',linetype = 2) +
                geom_point(aes(colour = Class, shape = Class)) +
                theme_bw() +
                ggtitle('MDS plot of a supervised\nrandom forest') +
                theme(plot.title = element_text(face = "bold"),
                      legend.title = element_text(face = "bold"),
                      axis.title = element_text(face = "bold"))
              
              if (!is.null(label)) {
              pl <- pl +
                geom_text_repel(aes(label = Label))
              }
              
              classLength <- distance$Class %>%
                unique() %>%
                length()
              
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
            
            if (length(x@preTreated) > 0) {
              dat <- analysis %>%
                preTreatedData()
              info <- analysis %>%
                preTreatedInfo()
            } else {
              dat <- analysis %>%
                rawData()
              info <-  analysis  %>%
                rawInfo()
            }
            y <- info %>%
              select(cls) %>%
              unlist() %>%
              factor()
            
            set.seed(seed)
            rf <- randomForest(dat,y = y,proximity = T,...)
            
            analysisPlot@data <- list(Data = dat,Info = info ,RFresults = rf, cls = cls, label = label)
            
            analysisPlot@plot <- analysisPlot@func(analysisPlot)
            
            return(analysisPlot)
          }
)