# false positive approximation for random forest selection frequencies
# @name selectionFrequencyFPR
# @description false positive approximation for random forest selection frequencies
# @param k variable selction frequency
# @param K The average number of binary tests made across a forest
# @param Tr The number of trees used to build the forest
# @param Ft Total number of variables
# @return A numeric value giving the false positive rate
# @author Jasen Finch
#' @importFrom stats dbinom

selectionFrequencyFPR <- function(k,K,Tr,Ft){
	
	nCm_ratio <- function(n1,m1,n2,m2){
		# Ender Konukoglu - 2014
		# 	E. Konukoglu and M. Ganz, "Approximate false positive rate control in selection 
		# 	frequency for Random Forests", 2014, arXiv:1410.2838
		
		if (m1 > n1) {
			return(0)
		} else {
			if (m2 > n2) {
				return(0)
			}
		}
		
		RN1 <- sum(log(seq(1,n1)))
		RM1 <- sum(log(seq(1,m1)))
		RNM1 <- sum(log(seq(1,n1 - m1)))
		RN2 <- sum(log(seq(1,n2)))
		RM2 <- sum(log(seq(1,m2)))
		RNM2 <- sum(log(seq(1,n2 - m2)))
		
		return(exp(RN1 - RM1 - RNM1 - RN2 + RM2 + RNM2))
	}
	
	
	SF_FPR <- function(k,Ft,Tr,K){
		Fn <- round(sqrt(Ft),0)
		p_Ct <- nCm_ratio(Ft - 1, Fn - 1, Ft, Fn)
		p <- p_Ct * dbinom(1,1,1/Fn)
		p <- dbinom(k,Tr*K,p)
		return(p)
	}
	
	if (k < 20) {
		val <- 20
	} else {
		if (k < round(Tr * K * 2/Ft)) {
			val <- round(Tr * K * 2/Ft)
		} else {
			val <- k
		}
	}
	p <- sapply(0:val,SF_FPR,Ft = Ft,Tr = Tr,K = K)
	
	p <- cumsum(p[length(p):1])
	p <- p[length(p):1]
	p <- round(p,7)
	return(p[k + 1])
	
}