#' @export

setMethod('plotRSD',signature = 'Analysis',
          function(analysis, cls = 'class', QCindex = 'QC', QCparameters = NULL){
            dat <- rawData(analysis)
            info <- dat$Info
            dat <- dat$Data
            
            classes <- unlist(unique(info[,cls]))[!(unlist(unique(info[,cls])) %in% QCindex)] %>%
              as.character()
            
            if (is.null(QCparameters)) {
              parameters <- analysisParameters('preTreat')
              parameters@preTreat <- list(
                remove = list(class = list(classes = cls)),
                occupancyFilter = list(maximum = list(cls = input$Column,occupancy = 2/3)),
                impute = list(all = list(occupancy = 2/3)),
                transform = list(TICnorm = list())
              )
            } else {
              parameters@preTreat <- QCparameters
            }
            
          }
)