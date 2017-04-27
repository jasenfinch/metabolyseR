#' @rdname preTreat
#' @importFrom missForest missForest
#' @importFrom plyr ldply
#' @importFrom stringr str_replace_all

setMethod("preTreat", signature = "Analysis",
          function(x){
            parameters <- x@parameters$preTreat
            dat <- list(Info = x@rawData$Info, Data = x@rawData$Data)
            
            for (i in 1:length(parameters)) {
              method <- preTreatMethods(names(parameters)[i])
              pars <- parameters[[i]]
              dat <- method(dat,pars)
            }
            
            x@preTreated <- dat
            x@log$preTreatment <- date()
            return(x)
          }
)