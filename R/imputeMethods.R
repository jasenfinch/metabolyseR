#' imputeMethods
#' @importFrom missForest missForest
#' @importFrom dplyr tbl_df bind_rows
#' @importFrom parallel makeCluster stopCluster parLapply
#' @export

imputeMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('all', 'class'),collapse = ' '))
  } else {
   methods <- list(
     
     all = function(dat, occupancy = 2/3){
         		dat$Data[which(dat == 0)] <- NA
         		capture.output(dat$Data <- missForest(dat$Data))
         		dat$Data <- dat$Data$ximp
         		return(dat)
     },
     
     class = function(dat, cls = 'class', occupancy = 2/3, nCores = 1){
       clus <- makeCluster(nCores)
       dat$Data <- parLapply(clus,as.character(sort(unique(unlist(dat$Info[,cls])))),function(y,dat,cls,occupancy){
         dat$Data <- data.frame(dat$Data)
         rownames(dat$Data) <- unlist(dat$Info[,'fileOrder'])
         dat$Data <- dat$Data[unlist(dat$Info[,cls] == y),]
         occ <- occMat(dat$Data,rep(1,nrow(dat$Data)))
         dat.1 <- dat$Data[,occ < occupancy]
         dat$Data <- dat$Data[,!(occ < occupancy)]
         dat$Data[dat$Data == 0] <- NA
         capture.output(dat$Data <- missForest(dat$Data))
         dat$Data <- dat$Data$ximp
         dat$Data <- cbind(dat.1,dat$Data)
         dat$Data <- t(dat$Data)
         dat$Data <- dat$Data[order(as.numeric(str_replace_all(rownames(dat$Data),'[:alpha:]',''))),]
         dat$Data <- t(dat$Data)
         dat$Data <- tbl_df(dat$Data)
         return(dat$Data)
       },dat = dat, cls = cls, occupancy = occupancy)
       stopCluster(clus)
       n <- unlist(lapply(dat$Data,rownames))
       dat$Data <- as.matrix(bind_rows(dat$Data))
       rownames(dat$Data) <- n
       dat$Data <- dat$Data[order(as.numeric(rownames(dat$Data))),]
       dat$Data <- tbl_df(data.frame(dat$Data))
       return(dat)
     }
   ) 
   method <- methods[[method]]
   return(method)
  }
}