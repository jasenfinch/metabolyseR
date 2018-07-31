#' plotRSD
#' @rdname plotRSD
#' @description Plot RSD distributions of raw data in quality control samples.
#' @param analysis object of class Analysis containing analysis results
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param QCparameters alternative parameters for QC sample pre-treatment. See details
#' @param modes split modes if present
#' @param histBins number of bins to use for histogram plotting
#' @details If QCparameters is set as \code{NULL}, the default QC pre-treatment parameters are used as given by \code{analysisParameters('preTreat')}. Alternative pre-treatment routines can be used by specifying an \code{AnalysisParameters} object for \code{QCparameters}.
#' @importFrom stringr str_extract
#' @importFrom purrr map
#' @importFrom stats median
#' @importFrom ggplot2 geom_histogram
#' @examples \dontrun{
#' 
#' files <- list.files(
#'   system.file(
#'     'DataSets/FIE-HRMS/BdistachyonEcotypes',
#'     package = 'metaboData'),
#'   full.names = TRUE)
#' 
#' info <- files[grepl('runinfo',files)]
#' files <- files[!grepl('runinfo',files)]
#' 
#' binDat <- binneRlyse::binneRlyse(files, 
#'                        info, 
#'                        parameters = binneRlyse::binParameters())
#' 
#' p <- new('AnalysisParameters')
#' 
#' analysis <- metabolyse(dplyr::bind_cols(binnedData(binDat)),info(binDat),p)
#' 
#' plotRSD(analysis)
#' }
#' @export

setMethod('plotRSD',signature = 'Analysis',
          function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, modes = T, histBins = 30){
            dat <- rawData(analysis)
            info <- rawInfo(analysis)
            
            classes <- unlist(unique(info[,cls]))[!(unlist(unique(info[,cls])) %in% QCidx)] %>%
              as.character()
            
            if (modes == T) {
              feat <- tibble(Feature = colnames(dat)) %>%
                mutate(Mode = str_extract(Feature,'[:alpha:]')) 
              dat <- feat %>%
                select(Mode) %>%
                unique() %>%
                unlist() %>%
                map(~{
                  dat[,feat$Mode == .]
                })
              names(dat) <- unique(feat$Mode)
            } else {
              dat <- list(dat)
              names(dat) <- ''
            }
            
            parameters <- analysisParameters('preTreat')
            if (is.null(QCparameters)) {
              parameters@preTreat <- list(
                remove = list(class = list(classes = classes)),
                occupancyFilter = list(maximum = list(cls = cls,occupancy = 2/3)),
                impute = list(all = list(occupancy = 2/3,nCores = detectCores()/2)),
                transform = list(TICnorm = list())
              )
            } else {
              parameters@preTreat <- QCparameters
            }
            
            rsd <- map(dat,~{
              d <- metabolyse(.,info = info, parameters = parameters,verbose = F) %>%
                preTreatedData() %>%
                gather('Feature','Intensity') %>%
                group_by(Feature) %>%
                summarise(RSD = sd(Intensity)/mean(Intensity))
              })  %>%
              bind_rows(.id = 'Mode')
            
            medians <- rsd %>%
              group_by(Mode) %>%
              summarise(Median = median(RSD))
            
            pl <- ggplot() +
              geom_histogram(data = rsd,aes_string(x = 'RSD'),fill = ptol_pal()(5)[2],colour = 'black',bins = histBins) +
              geom_vline(data = medians,aes_string(xintercept = 'Median'),linetype = 2,colour = 'red',size = 1) +
              theme_bw() +
              facet_wrap(~Mode)
            
            pl
            
          }
)