#' @rdname preTreat
#' @importFrom missForest missForest
#' @importFrom plyr ldply
#' @importFrom stringr str_replace_all

setMethod("preTreat", signature = "Analysis",
          function(x){
            parameters <- x@parameters$preTreat
            preTreated <- x@rawData$Data
            info <- x@rawData$Info
            
            if (!is.null(parameters$removeSample)) {
              preTreated <- preTreated[!(info$fileOrder %in% parameters$removeSample),]
              info <-  info[!(info$fileOrder %in% parameters$removeSample),]
            }
            
            if (!is.null(parameters$removeClass)) {
              preTreated <- preTreated[!(info[,parameters$cls] %in% parameters$removeClass),]
              info <-  info[!(info[,parameters$cls] %in% parameters$removeClass),]
            }
            
            if (parameters$QCfilter == T) {
              QC <- preTreated[which(info[,parameters$cls] == 'QC'),]
              QCocc <- occDrop(QC,rep(1,nrow(QC)),parameters$occupancy)
              if (parameters$QCimpute == T) {
                QCimp <- QCocc
                QCimp[which(QCimp == 0)] <- NA
                capture.output(QCimp <- missForest(QCimp))
                QCimp <- QCimp$ximp
              } else {
                QCimp <- QCocc
              }
              RSD <- apply(QCimp,2,function(y){sd(y)/mean(y)})
              QCrsd <- QCimp[,RSD <= parameters$RSDthresh]
              QC <- list(raw = QC, occupancyFiltered = QCocc, imputed = QCimp, rsdFiltered = QCrsd)
              preTreated <- preTreated[,colnames(QC$rsdFiltered)]
            }
            
            if (parameters$removeQC == T) {
              preTreated <- preTreated[-which(info[,parameters$cls] == 'QC'),]
              info <- info[-which(info[,parameters$cls] == 'QC'),]
            }
            
            if (parameters$QCfilter == F) {
              preTreated <- occDrop(preTreated,info[,parameters$cls],parameters$occupancy)
            }
            
            if (parameters$classImpute == T) {
              preTreated <- lapply(as.character(sort(unique(info[,parameters$cls]))),function(y,dat,info,cls,occupancy){
                rownames(dat) <- info$fileOrder
                dat <- dat[which(info[,cls] == y),]
                occ <- occMat(dat,rep(1,nrow(dat)))
                dat.1 <- dat[,which(occ < occupancy)]
                dat <- dat[,-which(occ < occupancy)]
                dat[which(dat == 0)] <- NA
                capture.output(dat <- missForest(dat))
                dat <- dat$ximp
                dat <- cbind(dat.1,dat)
                dat <- t(dat)
                dat <- dat[order(as.numeric(str_replace_all(rownames(dat),'[:alpha:]',''))),]
                dat <- t(dat)
                return(dat)
              },dat = preTreated,info = info, cls = parameters$cls, occupancy = parameters$occupancy)
              n <- unlist(lapply(preTreated,rownames))
              preTreated <- as.matrix(ldply(preTreated))
              rownames(preTreated) <- n
              preTreated <- preTreated[order(as.numeric(rownames(preTreated))),]
            }
            
            if (parameters$normTIC == T) {
              preTreated <- TICnorm(preTreated)	
            }
            if (parameters$logTrans == T) {
              preTreated <- log10(preTreated + parameters$add)
            }
            
            preTreated <- list(Info = info, Data = preTreated)
            if (parameters$QCfilter) {
              preTreated <- c(preTreated,QC = QC)
            }
            x@preTreated <- preTreated
            x@log$preTreatment <- date()
            return(x)
          }
)