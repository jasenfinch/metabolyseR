#' @importFrom missForest missForest
#' @importFrom stringr str_replace_all

setMethod("preTreat", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == T) {
              startTime <- proc.time()
              cat(blue('Pre-treatment'),cli::symbol$continue,'\r',sep = '') 
            }
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
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              cat(blue('Pre-treatment '),'\t\t',green(cli::symbol$tick),' ',elapsed,'\n',sep = '')
            }
            return(x)
          }
)