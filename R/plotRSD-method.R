#' plotRSD
#' @rdname plotRSD
#' @description Plot RSD distributions of raw data in quality control samples.
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param QCparameters alternative parameters for QC sample pre-treatment. See details
#' @param histBins number of bins to use for histogram plotting
#' @param title plot title
#' @param ... arguments to pass to the appropriate method
#' @details If QCparameters is set as \code{NULL}, the default QC pre-treatment parameters are used as given by \code{analysisParameters('preTreat')}. Alternative pre-treatment routines can be used by specifying an \code{AnalysisParameters} object for \code{QCparameters}.
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @importFrom stats median
#' @importFrom ggplot2 geom_histogram geom_text geom_density
#' @importFrom patchwork plot_layout
#' @examples \dontrun{
#' 
#' library(metaboData)
#' library(binneR)
#' 
#' files <- filePaths("FIE-HRMS","BdistachyonEcotypes")
#' 
#' info <- runinfo("FIE-HRMS","BdistachyonEcotypes")
#' 
#' binDat <- binneRlyse(files, 
#'                        info, 
#'                        parameters = detectParameters(files))
#' 
#' p <- analysisParameters('preTreat')
#' 
#' analysis <- metabolyse(binnedData(binDat)$n,info(binDat),p)
#' 
#' plotRSD(analysis)
#' }
#' @export

setMethod('plotRSD',signature = 'AnalysisData',
          function(analysis, cls = 'class'){
            
            if (clsExtract(analysis,cls) %>% is.numeric()) {
              stop("Argument 'cls' should be either a factor or character",call. = FALSE)  
            }
            
            x <- rsd(analysis,cls = cls)
            
            d <- ggplot(x,aes_string(x = 'RSD',colour = cls,group = cls)) +
              geom_density() +
              theme_bw() +
              labs(title = 'Density distrubution') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom')
            
            cs <- x %>%
              group_by_at(c(cls,'RSD')) %>%
              summarise(sum = n()) %>%
              mutate(cs = cumsum(sum))
            
            csDist <- ggplot(cs,aes_string(x = 'RSD',y = 'cs',colour = cls)) + 
              geom_line() + 
              theme_bw() +
              labs(title = 'Cumulative distribution',
                   y = 'Cumulative frequency') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'),
                    legend.title = element_text(face = 'bold'),
                    legend.position = 'bottom')
            
            if (length(clsExtract(analysis,cls) %>% unique()) < 12) {
              d <- d +
                scale_colour_ptol()
              
              csDist <- csDist +
                scale_colour_ptol()
            }
            
            pl <- d + csDist
            
            return(pl)
          }
)

#' @rdname plotRSD
#' @export

setMethod('plotRSD',signature = 'Analysis',
          function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, histBins = 30, title = ''){
            
            analysis <- raw(analysis)
            
            d <- dat(analysis)
            i <- sinfo(analysis)
            
            if (is.null(QCparameters)) {
              QCparameters <- analysisParameters('preTreat')
              QCparameters@preTreat <- list(
                keep = list(classes = list(cls = cls,classes = QCidx)),
                occupancyFilter = list(maximum = list(cls = cls,occupancy = 2/3)),
                impute = list(all = list(occupancy = 2/3,parallel = 'variables',nCores = detectCores() * 0.75,clusterType = getClusterType(),seed = 1234))
              )
            }
            
            rsd <- d %>% 
              metabolyse(.,info = i, parameters = QCparameters,verbose = FALSE) %>%
              dat(type = 'pre-treated') %>%
              gather('Feature','Intensity') %>%
              group_by(Feature) %>%
              summarise(RSD = sd(Intensity)/mean(Intensity))
            
            cs <- rsd %>%
              group_by(RSD) %>%
              summarise(sum = n()) %>%
              mutate(cs = cumsum(sum))
            
            csDist <- ggplot(cs,aes(x = RSD,y = cs)) + 
              geom_line(colour = ptol_pal()(1)) + 
              theme_bw() +
              labs(title = 'Cumulative distribution',
                   y = 'Cumulative frequency') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'))
            
            medians <- rsd %>%
              summarise(Median = median(RSD)) %>%
              mutate(Label = str_c('Median: ',Median %>% round(3)),
                     x = Inf,
                     y = Inf,
                     hjust = 1.5,
                     vjust = 1.3)
            
            RSDdist <- ggplot() +
              geom_histogram(data = rsd,aes_string(x = 'RSD'),fill = ptol_pal()(5)[2],colour = 'black',bins = histBins) +
              geom_vline(data = medians,aes_string(xintercept = 'Median'),
                         linetype = 2,colour = 'red',size = 1) +
              geom_text(data = medians,
                        aes_string(x = 'x', y = 'y', label = 'Label',hjust = 'hjust',vjust = 'vjust'),
                        size = 3) +
              theme_bw() +
              labs(title = 'Frequency distribution',
                   y = 'Frequency',
                   caption = 'Red dash line shows median RSD value') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'))
            
            RSDdist + 
              csDist + 
              plot_annotation(title = title,
                              theme = theme(plot.title = element_text(face = 'bold')))
            
          }
)