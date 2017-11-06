#' @importFrom stringr str_extract
#' @export

setMethod('plotRSD',signature = 'Analysis',
          function(analysis, cls = 'class', QCindex = 'QC', QCparameters = NULL, modes = T){
            dat <- rawData(analysis)
            info <- dat$Info
            dat <- dat$Data
            
            classes <- unlist(unique(info[,cls]))[!(unlist(unique(info[,cls])) %in% QCindex)] %>%
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
            
            if (is.null(QCparameters)) {
              parameters <- analysisParameters('preTreat')
              parameters@preTreat <- list(
                remove = list(class = list(classes = classes)),
                occupancyFilter = list(maximum = list(cls = cls,occupancy = 2/3)),
                impute = list(all = list(occupancy = 2/3)),
                transform = list(TICnorm = list())
              )
            } else {
              parameters@preTreat <- QCparameters
            }
            
            rsd <- map(dat,~{
              d <- metabolyse(.,info = info, parameters = parameters) %>%
                preTreatedData()
              d$Data %>%
                gather('Feature','Intensity') %>%
                group_by(Feature) %>%
                summarise(RSD = sd(Intensity)/mean(Intensity))
              })  %>%
              bind_rows(.id = 'Mode')
            
            medians <- rsd %>%
              group_by(Mode) %>%
              summarise(Median = median(RSD))
            
            pl <- ggplot() +
              geom_histogram(data = rsd,aes_string(x = 'RSD'),fill = ptol_pal()(5)[2],colour = 'black') +
              geom_vline(data = medians,aes_string(xintercept = 'Median'),linetype = 2,colour = 'red',size = 1) +
              theme_bw() +
              facet_wrap(~Mode)
            
            pl
            
          }
)