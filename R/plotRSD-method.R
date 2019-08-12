#' plotRSD
#' @rdname plotRSD
#' @description Plot RSD distributions of raw data in quality control samples.
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param QCparameters alternative parameters for QC sample pre-treatment. See details
#' @param modes split modes if present
#' @param histBins number of bins to use for histogram plotting
#' @param title plot title
#' @details If QCparameters is set as \code{NULL}, the default QC pre-treatment parameters are used as given by \code{analysisParameters('preTreat')}. Alternative pre-treatment routines can be used by specifying an \code{AnalysisParameters} object for \code{QCparameters}.
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @importFrom stats median
#' @importFrom ggplot2 geom_histogram geom_text
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
#'                        parameters = binParameters(scans = detectInfusionScans(files)))
#' 
#' p <- analysisParameters('preTreat')
#' 
#' analysis <- metabolyse(binnedData(binDat)$n,info(binDat),p)
#' 
#' plotRSD(analysis)
#' }
#' @export

setMethod('plotRSD',signature = 'AnalysisData',
          function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, modes = T, histBins = 30, title = 'Relative standard deviation distributions'){
            d <- dat(analysis)
            i <- sinfo(analysis)
            
            if (modes == T) {
              feat <- tibble(Feature = colnames(d)) %>%
                mutate(Mode = str_extract(Feature,'[:alpha:]')) 
              d <- feat %>%
                select(Mode) %>%
                unique() %>%
                unlist() %>%
                map(~{
                  d[,feat$Mode == .]
                })
              names(d) <- unique(feat$Mode)
            } else {
              d <- list(d)
              names(d) <- ''
            }
            
            
            if (is.null(QCparameters)) {
              QCparameters <- analysisParameters('preTreat')
              QCparameters@preTreat <- list(
                keep = list(classes = list(cls = cls,classes = QCidx)),
                occupancyFilter = list(maximum = list(cls = cls,occupancy = 2/3)),
                impute = list(all = list(occupancy = 2/3,parallel = 'variables',nCores = detectCores() * 0.75,clusterType = getClusterType(),seed = 1234))
              )
            }
            
            rsd <- map(d,~{
              metabolyse(.,info = i, parameters = QCparameters,verbose = F) %>%
                preTreatedData() %>%
                gather('Feature','Intensity') %>%
                group_by(Feature) %>%
                summarise(RSD = sd(Intensity)/mean(Intensity))
            })  %>%
              bind_rows(.id = 'Mode')
            
            medians <- rsd %>%
              group_by(Mode) %>%
              summarise(Median = median(RSD)) %>%
              mutate(Label = str_c('Median: ',Median %>% round(3)),
                     x = Inf,
                     y = Inf,
                     hjust = 1.5,
                     vjust = 1.3)
            
            pl <- ggplot() +
              geom_histogram(data = rsd,aes_string(x = 'RSD'),fill = ptol_pal()(5)[2],colour = 'black',bins = histBins) +
              geom_vline(data = medians,aes_string(xintercept = 'Median'),linetype = 2,colour = 'red',size = 1) +
              geom_text(data = medians,
                        aes_string(x = 'x', y = 'y', label = 'Label',hjust = 'hjust',vjust = 'vjust'),size = 3) +
              theme_bw() +
              facet_wrap(~Mode) +
              labs(title = title,
                   y = 'Count',
                   caption = 'Red dash line shows median RSD value') +
              theme(plot.title = element_text(face = 'bold'),
                    axis.title = element_text(face = 'bold'))
            
            pl
          }
)

#' @rdname plotRSD
#' @export

setMethod('plotRSD',signature = 'Analysis',
          function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, modes = T, histBins = 30, title = 'Relative standard deviation distributions'){
            plotRSD(analysis@rawData,cls = cls,QCidx = QCidx,QCparameters = QCparameters,modes = modes,histBins = histBins,title = title)
          }
)