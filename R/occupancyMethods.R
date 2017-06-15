#' occupancyMethods
#' @export

occupancyMethods <- function(method = NULL){
  if (is.null(method)) {
    cat('Available Methods:',paste(c('maximum', 'minimum'),collapse = ' '))
  } else {
    methods <- list(
      maximum = function(dat,cls = 'class', occupancy = 2/3){
        mat <- occMat(as.matrix(dat$Data),unlist(dat$Info[,cls]))
        occ <- apply(mat,2,max)
        dat$Data <- dat$Data[,which(occ >= occupancy)]
        return(dat)
      }, 
      minimum = function(dat,cls = 'class', occupancy = 2/3){
        mat <- occMat(as.matrix(dat$Data),unlist(dat$Info[,cls]))
        occ <- apply(mat,2,min)
        dat$Data <- dat$Data[,which(occ >= occupancy)]
        return(dat)
      }
    )
    method <- methods[[method]]
    return(method)
  }
}