# Aggregate correlations into lists for each variable

corLists <-
	function(corMat) {
	  lists <- apply(corMat,2,function(x,n){
	    names(x) <- n
	    x <- x[!(x == 0)]
	    x <- sort(x,decreasing = T)
	    x <- data.frame(names(x),x)
	    colnames(x) <- c('mz',"r")
	    rownames(x) <- NULL
	    return(x)
	  },n = rownames(corMat))
	  
		names(lists) <- colnames(corMat)
		return(lists)
	}