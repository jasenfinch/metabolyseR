
occupancyMethods <- function(method = NULL, description = F){
    
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
    
  descriptions = list(
    maximum = list(description = 'maximum thresholded class occupancy filtering', 
                   arguments = c(cls = 'info column to use for class labels', 
                                                   occupancy = 'occupancy threshold')),
    minimum = list(description = 'minimum thresholded class occupancy filtering', 
                   arguments = c(cls = 'info column to use for class labels', 
                                                   occupancy = 'occupancy threshold'))
    )
  
    if (description == F) {
      if (is.null(method)) {
        method <- methods
      } else {
        method <- methods[[method]]
      }
    } else {
      if (is.null(method)) {
        method <- descriptions
      } else {
        method <- descriptions[[method]]
      }
    }
    return(method)
}