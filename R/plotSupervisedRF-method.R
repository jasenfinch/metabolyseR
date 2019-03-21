#' plotSupervisedRF
#' @rdname plotSupervisedRF
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for sample classes
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param ROC should reciever-operator characteristics be plotted?
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @param ... additional parameters to pass to randomForest
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 stat_ellipse coord_fixed scale_fill_manual geom_abline geom_line
#' @importFrom ggthemes scale_fill_ptol
#' @importFrom patchwork wrap_plots
#' @importFrom ROCR prediction performance
#' @importFrom magrittr set_names
#' @importFrom randomForest margin
#' @examples 
#' library(metaboData)
#' data(abr1)
#' p <- analysisParameters('preTreat')
#' p@preTreat <- list(
#'   occupancyFilter = list(maximum = list()),
#'   transform = list(TICnorm = list())
#' )
#' analysis <- metabolyse(abr1$neg,abr1$fact,p)  
#' 
#' plotSupervisedRF(analysis,label = 'name')
#' @export

setMethod('plotSupervisedRF', signature = 'Analysis',
          function(analysis, cls = 'class', label = NULL, ellipses = T, ROC = T, seed = 1234, title = 'MDS plot of a supervised random forest', legendPosition = 'bottom', labelSize = 2, ...){
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
                geom_vline(xintercept = 0,colour = 'lightgray',linetype = 2)
              
              if (isTRUE(ellipses)) {
                pl <- pl +
                  stat_ellipse(aes(fill = Class),alpha = 0.3,geom = 'polygon',type = 'norm')
              }
              
              if (!is.null(label)) {
              pl <- pl +
                geom_text_repel(aes(label = Label),size = labelSize)
              }
              
              classLength <- distance$Class %>%
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
                geom_point(aes(colour = Class, shape = Class)) +
                theme_bw() +
                theme(plot.title = element_text(face = "bold"),
                      legend.title = element_text(face = "bold"),
                      axis.title = element_text(face = "bold"),
                      legend.position = legendPosition) +
                labs(title = title,
                     x = 'Dimension 1',
                     y = 'Dimension 2',
                     caption = str_c('Margin: ',rf %>% margin() %>% mean() %>% round(3))) +
                coord_fixed()
              
              if (ROC == T) {
                rocTable <- analysisPlot@data$roc %>%
                  map(~{
                    tibble(fpr = .$performance@x.values[[1]],
                           tpr = .$performance@y.values[[1]])
                  })  %>%
                  bind_rows(.id = 'Class')
                
                rocpl <- ggplot(rocTable,aes(x = fpr,y = tpr,colour = Class)) +
                  geom_abline(intercept = 0,linetype = 2,colour = 'grey') +
                  geom_line() +
                  theme_bw(base_size = 12) +
                  labs(title = 'ROC Curves',x = '1 - Specificity',
                       y = 'Sensitivity') +
                  coord_fixed() +
                  theme(plot.title = element_text(face = "bold"),
                        legend.title = element_text(face = "bold"),
                        axis.title = element_text(face = "bold"),
                        legend.position = legendPosition)
                
                classLength <- distance$Class %>%
                  unique() %>%
                  length()
                
                if (classLength <= 12) {
                  rocpl <- rocpl + 
                    scale_colour_ptol()
                } else {
                  if (classLength %% 12 == 0) {
                    pal <- rep(ptol_pal()(12),classLength / 12)
                  } else {
                    pal <- c(rep(ptol_pal()(12),floor(classLength / 12)),ptol_pal()(12)[1:(classLength %% 12)])
                  }
                  rocpl <- rocpl + 
                    scale_colour_manual(values = pal)
                }
                  
                pl <- wrap_plots(pl, rocpl)
              }
              pl
            }
            
            if (length(analysis@preTreated) > 0) {
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
            
            roc <- {
              classes <- rf$classes
              map(1:length(classes),~{
                cls <- classes[.]
                true_values <- ifelse(rf$y == cls,1,0)
                pred <- prediction(rf$votes[,.],true_values)
                perf <- performance(pred, "tpr", "fpr")
                auc <- performance(pred,'auc') %>%
                  .@y.values %>% .[[1]]
                list(performance = perf,auc = auc)
              }) %>%
                set_names(classes)
            }
              
            analysisPlot@data <- list(Data = dat,Info = info ,RFresults = rf,roc = roc, cls = cls, label = label)
            
            analysisPlot@plot <- list(analysisPlot@func(analysisPlot))
            
            return(analysisPlot)
          }
)