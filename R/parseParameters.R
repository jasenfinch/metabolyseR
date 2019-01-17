#' parseParameters
#' @description parse .yaml file containing analysis parameters.
#' @param path file path of .yaml file to parse
#' @importFrom yaml read_yaml
#' @importFrom stringr str_remove_all
#' @examples 
#' 
#' paramFile <- system.file('defaultParameters.yaml',package = 'metabolyseR')
#' p <- parseParameters(paramFile)
#' p
#' 
#' @export

parseParameters <- function(path){
  par <- read_yaml(path)
  
  par <- par %>%
    map(~{
      names(.) <- str_remove_all(names(.),'[\\b\\d+\\b]')
      return(.)
    })
  
  if ('preTreat' %in% names(par)) {
    par$preTreat <- par$preTreat %>%
      map(~{
        map(.,~{
          map(.,~{
            if (is.list(.)) {
              . <- unlist(.,use.names = F)
            }
            return(.)
          })
        })
      })
  }
  
  ap <- new('AnalysisParameters')
  
  elements <- slotNames(ap)
  
  for (i in elements) {
    if (i %in% names(par)) {
      slot(ap,i) <- par[[i]] 
    }
  }
  
  return(ap)
}
