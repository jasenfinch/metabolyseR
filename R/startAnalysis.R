#' startAnalysis 
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