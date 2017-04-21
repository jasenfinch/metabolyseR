# Total Ion Count Normalisation.
# @name TICnorm
# @description Normalise FIE-HRMS spectra to their total ion count.
# @param dat A data.frame.
# @return A TIC normalised data.frame.
# @author Jasen Finch

TICnorm <- function(dat){
	dat <- apply(dat,2,function(x,y){x/y},y = rowSums(dat))
	return(dat)
}