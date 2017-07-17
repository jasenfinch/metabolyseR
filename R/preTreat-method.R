#' @importFrom missForest missForest
#' @importFrom stringr str_replace_all

setMethod("preTreat", signature = "Analysis",
          function(x){
            params <- x@parameters@preTreat
            dat <- list(Data = x@rawData$Data, Info = x@rawData$Info)
            
            for (i in 1:length(params)) {
              method <- preTreatMethods(names(params)[i])
              m <- method(names(params[[i]]))
              for (j in 1:length(m)) {
                if (!(length(params[[i]][[j]]) == 0)) {
                  newPars <- formals(m[[j]])
                  newPars[names(params[[i]][[j]])] <- params[[i]][[j]]
                  formals(m[[j]]) <- newPars
                }
                dat <- m[[j]](dat)
              }
            }
            x@preTreated <- list(Data = as_tibble(dat$Data), Info = as_tibble(dat$Info))
            x@log$preTreatment <- date()
            return(x)
          }
)