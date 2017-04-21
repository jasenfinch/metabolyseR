# Class occupancy filtering.
# @name occDrop
# @description Drop variables without a maximum class occupancy above a given proportion.
# @param dat A data.frame.
# @param cls A vector denoting the class of each observation in the data.frame dat.
# @param proportion The proportion threshold between 0 and 1, above which a varible must contain at least one class.
# @return An occupancy filtered data.frame
# @author Jasen Finch

occDrop <- function(dat,cls,proportion){	
	mat <- occMat(dat,cls)
	max.occ <- apply(mat,2,max)
	dat <- dat[,which(max.occ >= proportion)]
	return(dat)
}