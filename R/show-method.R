#' show-AnalysisParameters
#' @description show method for AnalysisParameters class. 
#' @param object S4 object of class AnalysisParameters
#' @importFrom methods show
#' @importFrom plyr ldply
#' @export

setMethod('show',signature = 'AnalysisParameters',
          function(object){
            
            elements <- slotNames(object)
            elements <- elements[sapply(elements, function(x,object){
              length(slot(object,x)) != 0
            },object = object) == T]
            names(elements) <- elements
            
            if ('preTreat' %in% elements) {
              preTreat <- slot(object,'preTreat')
              preTreat <- lapply(preTreat,function(x){
                x <- lapply(x,function(y){
                  if (length(y) > 0) {
                    n <- paste('\t\t',names(y),' = ',y,'\n',sep = '')
                    n <- paste(n,collapse = '')
                  } else {
                    n <- ''
                  }
                  return(n)
                })
                x <- paste('\t',names(x),'\n',x,sep = '')
                x <- paste(x,collapse = '')
                return(x)
              })
              preTreat <- paste(preTreat,collapse = '')
            }
            
            if ('classification' %in% elements) {
              classification <- slot(object,'classification')
              classification[sapply(classification,length) == 1] <- lapply(names(classification)[sapply(classification,length) == 1],
                                                                           function(x,object){
                                                                             paste('\t',x,' = ',object[[x]],'\n',sep = '')
                                                                           },object = classification)
              classification[sapply(classification,length) > 1] <- lapply(names(classification)[sapply(classification,length) > 1],
                                                                          function(x,object){
                                                                            n <- paste('\t\t',names(object[[x]]),' = ',object[[x]],'\n',sep = '')
                                                                            n <- paste(n,collapse = '')
                                                                            n <- paste('\t',x,'\n',n,sep = '')
                                                                          },object = classification)
              classification <- paste(classification,collapse = '')
            }
            
            if ('featureSelection' %in% elements) {
              featureSelection <- slot(object,'featureSelection')
              featureSelection[sapply(featureSelection,class) != 'list'] <- lapply(names(featureSelection)[sapply(featureSelection,class) != 'list'],
                                                                                   function(x,object){
                                                                                     paste('\t',x,' = ',object[[x]],'\n',sep = '')
                                                                                   },object = featureSelection)
              featureSelection[sapply(featureSelection,class) == 'list'] <- lapply(names(featureSelection)[sapply(featureSelection,class) == 'list'],
                                                                                   function(x,object){
                                                                                     object <- object[[x]]
                                                                                     n <- lapply(object,function(y){
                                                                                       paste('\t\t',names(y),' = ',y,'\n',sep = '')
                                                                                     })
                                                                                     n <- paste(n,collapse = '')
                                                                                     n <- paste('\t',x,'\n',n,sep = '')
                                                                                   },object = featureSelection)
              featureSelection <- paste(featureSelection,collapse = '')
            }
            
            if ('correlations' %in% elements) {
              correlations <- slot(object,'correlations')
              correlations <- paste(names(correlations), correlations,sep = ' = ')
              correlations <- paste('\t',correlations,sep = '')
              correlations <- paste(correlations,collapse = '\n')
            }
            
            elements <- lapply(elements,function(x){
              get(x)
            })
            elements <- paste(names(elements),elements,sep = '\n')
            cat(elements,sep = '\n')
          }
)

#' show-Analysis
#' @description show method for Analysis class. 
#' @param object S4 object of class Analysis
#' @importFrom methods show
#' @export

setMethod('show',signature = 'Analysis',
          function(object){
            elements <- slotNames(object)
            elements <- elements[4:length(elements)]
            elements <- elements[sapply(elements, function(x,object){
              length(slot(object,x)) != 0
            },object = object) == T]
            names(elements) <- elements
            
            time <- object@log$analysis
            
            rD <- rawData(object)
            rD <- paste('\t\tNo. samples = ',nrow(rD$Info),'\n','\t\tNo. variables = ',ncol(rD$Data),'\n',sep = '')
            
            cat('\nAnalysis:\n','\t',time,'\n',sep = '')
            cat('\n\tRaw Data:\n',rD,sep = '')
            
            if ('preTreated' %in% elements) {
              time <- object@log$preTreatment
              pD <- preTreatedData(object)
              pD <- paste('\t\tNo. samples = ',nrow(pD$Info),'\n','\t\tNo. variables = ',ncol(pD$Data),'\n',sep = '')
              
              cat('\n\tPre-treated Data:\n','\t\t',time,'\n',pD,sep = '')
            }
            
            if ('classification' %in% elements) {
              time <- object@log$classification
              cR <- classificationResults(object)
              cR <- paste('\t\tMethods = ',paste(unique(cR$Method,collapse = '\t')),'\n','\t\tNo. pairwises = ',length(unique(cR$Pairwise)),'\n',sep = '')
              
              cat('\n\tClassification:\n','\t\t',time,'\n',cR,sep = '')
            }
            
            if ('featureSelection' %in% elements) {
              time <- object@log$featureSelection
              fsR <- featureSelectionResults(object)
              fsR <- paste('\t\tMethods = ',paste(unique(fsR$Method,collapse = '\t')),'\n','\t\tNo. pairwises = ',length(unique(fsR$Pairwise)),'\n',sep = '')
              
              cat('\n\tFeature selection:\n','\t\t',time,'\n',fsR,sep = '')
            }
            
            if ('correlations' %in% elements) {
              time <- object@log$correlations
              corR <- correlationResults(object)
              corR <- paste('\t\tNo. correlations = ',nrow(corR),'\n',sep = '')
              cat('\n\tCorrelations:\n','\t\t',time,'\n',corR,sep = '')
            }
            
          }
)