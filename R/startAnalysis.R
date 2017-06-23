#' startAnalysis 
#' @examples 
#' library(FIEmspro)
#' data(abr1)
#' p <- parameters()
#' p@preTreat <- list(
#'     occupancyFilter = list(maximum = list()),
#'     transform = list(TICnorm = list())
#' )
#' analysis <- startAnalysis(abr1$neg,abr1$fact,p) %>% 
#' preTreat 
#' @export

startAnalysis <- function(data,info,params = parameters()){
  new('Analysis',
      log = list(analysis = date()),
      parameters = params,
      rawData = list(Info = info,Data = data),
      preTreated = list(),
      classification = list(),
      featureSelection = list()
  )
}