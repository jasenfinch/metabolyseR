#' parseParameters
#' @description parse .yaml file containing analysis parameters.
#' @param path file path of .yaml file to parse
#' @importFrom yaml read_yaml
#' @examples 
#' 
#' paramFile <- system.file('defaultParameters.yaml',package = 'metabolyseR')
#' p <- parseParameters(paramFile)
#' p
#' 
#' @export

parseParameters <- function(path){
  par <- read_yaml(path)
  
  ap <- new('AnalysisParameters')
  
  elements <- slotNames(ap)
  
  for (i in elements) {
    slot(ap,i) <- par[[i]]
  }
  
  return(ap)
}
