#' imputeMethods
#' @importFrom missForest missForest
#' @export

imputeMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('all', 'class'),collapse = ' '))
  } else {
   methods <- list(
     
     all = function(dat, occupancy = 2/3){
         		dat$Data[which(dat == 0)] <- NA
         		dat$Data <- missForest(dat$Data)
         		dat$Data <- dat$Data$ximp
         		return(dat)
     },
     
     class = function(dat, cls = 'Class', occupancy = 2/3){
       
       dat$Data <- lapply(as.character(sort(unique(dat$Info[,cls]))),function(y,dat,cls,occupancy){
         rownames(dat$Data) <- dat$Info$fileOrder
         dat$Data <- dat[which(cls == y),]
         occ <- occMat(dat$Data,rep(1,nrow(dat$Data)))
         dat.1 <- dat$Data[,which(occ < occupancy)]
         dat$Data <- dat$Data[,-which(occ < occupancy)]
         dat$Data[which(dat$Data == 0)] <- NA
         capture.output(dat$Data <- missForest(dat$Data))
         dat$Data <- dat$Data$ximp
         dat$Data <- cbind(dat.1,dat$Data)
         dat$Data <- t(dat$Data)
         dat$Data <- dat$Data[order(as.numeric(str_replace_all(rownames(dat$Data),'[:alpha:]',''))),]
         dat$Data <- t(dat$Data)
         return(dat$Data)
       },dat = dat, cls = cls, occupancy = occupancy)
       n <- unlist(lapply(dat$Data,rownames))
       dat$Data <- as.matrix(ldply(dat$Data))
       rownames(dat$Data) <- n
       dat$Data <- dat$Data[order(as.numeric(rownames(dat$Data))),]
     }
   ) 
   method <- methods[[method]]
   return(method)
  }
}