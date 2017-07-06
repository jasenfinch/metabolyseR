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
                  n <- paste(names(y),y,sep = ' = ')
                  n <- sapply(n,function(y){paste('\t\t',y,'\n',sep = '')})
                  n <- paste(n,collapse = '\t')
                  n <- paste('\t',n,sep = '')
                  return(n)
                })
                x <- paste(names(x),x,sep = '\n')
                x <- paste(x,collapse = '\t\t')
                x <- paste('\n\t\t',x,sep = '')
                return(x)
              })
              preTreat <- paste(names(preTreat),preTreat,sep = '\t')
              preTreat <- paste(preTreat,collapse = '\n\t')
              preTreat <- paste('\t',preTreat,sep = '')
            }
            
            if ('classification' %in% elements) {
              classification <- slot(object,'classification')
              classification[sapply(classification,length) == 1] <- lapply(names(classification)[sapply(classification,length) == 1],
                                                                          function(x,object){
                                                                            paste(x,object[[x]],sep = ' = ')
                                                                          },object = classification)
              classification[sapply(classification,length) > 1] <- lapply(names(classification)[sapply(classification,length) > 1],
                                                                          function(x,object){
                                                                            n <- paste(names(object[[x]]),object[[x]],sep = ' = ')
                                                                            n <- paste(n,collapse = '\n\t\t')
                                                                            n <- paste('\t\t',n,sep = '')
                                                                            n <- paste(x,n,sep = '\n')
                                                                          },object = classification)
              classification <- paste(classification,collapse = '\n\t')
              classification <- paste('\t',classification,sep = '')
            }
            
            if ('featureSelection' %in% elements) {
              featureSelection <- slot(object,'featureSelection')
              featureSelection[sapply(featureSelection,length) == 1] <- lapply(names(featureSelection)[sapply(featureSelection,length) == 1],
                                                                           function(x,object){
                                                                             paste(x,object[[x]],sep = ' = ')
                                                                           },object = featureSelection)
              featureSelection[sapply(featureSelection,length) > 1] <- lapply(names(featureSelection)[sapply(featureSelection,length) > 1],
                                                                          function(x,object){
                                                                            n <- paste(names(object[[x]]),object[[x]],sep = ' = ')
                                                                            n <- paste(n,collapse = '\n\t\t')
                                                                            n <- paste('\t\t',n,sep = '')
                                                                            n <- paste(x,n,sep = '\n')
                                                                          },object = featureSelection)
              featureSelection <- paste(featureSelection,collapse = '\n\t')
              featureSelection <- paste('\t',featureSelection,sep = '') 
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