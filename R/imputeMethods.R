#' imputeMethods
#' @importFrom missForest missForest
#' @export

imputeMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('all', 'class'),collapse = ' '))
  } else {
   methods <- list(
     
     all = function(dat,occupancy = 2/3){
         		dat[which(dat == 0)] <- NA
         		dat <- missForest(dat)
         		dat <- dat$ximp
         		return(dat)
     },
     
     class = function(dat,cls,occupancy = 2/3){
       
       dat <- lapply(as.character(sort(unique(cls))),function(y,dat,cls,occupancy){
         rownames(dat) <- info$fileOrder
         dat <- dat[which(cls == y),]
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
       },dat = dat, cls = cls, occupancy = occupancy)
       n <- unlist(lapply(dat,rownames))
       dat <- as.matrix(ldply(dat))
       rownames(dat) <- n
       dat <- dat[order(as.numeric(rownames(dat))),]
     }
   ) 
   method <- methods[[method]]
   return(method)
  }
}