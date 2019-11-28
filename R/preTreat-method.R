#' @importFrom missForest missForest
#' @importFrom stringr str_replace_all

setMethod("preTreat", signature = "Analysis",
          function(x){
            verbose <- x@log$verbose
            if (verbose == T) {
              startTime <- proc.time()
              message(blue('Pre-treatment '),cli::symbol$continue,'\r',appendLF = FALSE) 
            }
            params <- x@parameters@preTreat
            d <- analysisData(rawData(x), rawInfo(x))
            
            for (i in 1:length(params)) {
              method <- preTreatMethods(names(params)[i])
              m <- method(names(params[[i]]))
              for (j in 1:length(m)) {
                newPars <- formals(m[[j]])
                if (!(length(params[[i]][[j]]) == 0)) {
                  newPars[names(params[[i]][[j]])] <- params[[i]][[j]]
                }
                newPars[[1]] <- d
                d <- do.call(m[[j]],newPars %>% as.list())
              }
            }
            x@preTreated <- d
            x@log$preTreatment <- date()
            
            if (verbose == T) {
              endTime <- proc.time()
              elapsed <- {endTime - startTime} %>%
                .[3] %>%
                round(1) %>%
                seconds_to_period() %>%
                str_c('[',.,']')
              message('\r',blue('Pre-treatment '),'\t',green(cli::symbol$tick),' ',elapsed)
            }
            return(x)
          }
)