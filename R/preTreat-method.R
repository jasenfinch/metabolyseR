#' @rdname preTreat
#' @importFrom missForest missForest
#' @importFrom plyr ldply
#' @importFrom stringr str_replace_all

setMethod("preTreat", signature = "Analysis",
          function(x){
            params <- x@parameters@preTreat
            dat <- list(dat = x@rawData$Data, info = x@rawData$Info)
            
            for (i in 1:length(params)) {
              method <- preTreatMethods(names(params)[i])
              for (j in 1:length(params[[i]])) {
                m <- method(names(params[[i]])[[j]])
                # if (!is.null(params[[i]][[j]])) {
                #   newPars <- formals(m)
                #   newPars[names(params[[i]][[j]])] <- params[[i]][[j]]
                #   formals(m) <- newPars
                # }
                # m <- m(dat)
                print(m)
              }
            }
            x@preTreated <- preTreated
            x@log$preTreatment <- date()
            return(x)
          }
)