#' show-AnalysisParameters
#' @description show method for AnalysisParameters class. 
#' @param object S4 object of class AnalysisParameters
#' @importFrom methods show
#' @importFrom purrr map_dbl map_chr
#' @export

setMethod('show',signature = 'AnalysisParameters',
          function(object){
            
            elements <- analysisElements()
            elements <- elements[sapply(elements, function(x,object){
              length(slot(object,x)) != 0
            },object = object) == T]
            names(elements) <- elements
            
            if ('pre-treatment' %in% elements) {
              `pre-treatment` <- slot(object,'pre-treatment')
              `pre-treatment` <- lapply(`pre-treatment`,function(x){
                x <- lapply(x,function(y){
                  if (length(y) > 0) {
                    n <- paste('\t\t\t',names(y),' = ',y,'\n',sep = '')
                    n <- paste(n,collapse = '')
                  } else {
                    n <- ''
                  }
                  return(n)
                })
                x <- paste('\t\t',names(x),'\n',x,sep = '')
                x <- paste(x,collapse = '')
                return(x)
              })
              `pre-treatment` <- paste('\t',names(`pre-treatment`),'\n',`pre-treatment`,sep = '')
              `pre-treatment` <- paste(`pre-treatment`,collapse = '')
            }
            
            if ('modelling' %in% elements) {
              modelling <- slot(object,'modelling')
              modelling <- lapply(modelling,function(x){
                if (length(x) > 0) {
                  x <- paste('\t\t',names(x),' = ',x,'\n',sep = '')
                  x <- paste(x,collapse = '')
                } else {
                  x <- ''
                }
              })
              modelling <- paste('\t',names(modelling),'\n',modelling,sep = '')
              modelling <- paste(modelling,collapse = '')
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
            cat(yellow('Parameters:'),elements,sep = '\n')
          }
)

#' show-Analysis
#' @description show method for Analysis class. 
#' @param object S4 object of class Analysis
#' @importFrom methods show
#' @importFrom crayon blue bold red
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
            rI <- rawInfo(object)
            rD <- paste('\t\tNo. samples = ',nrow(rI),'\n','\t\tNo. variables = ',ncol(rD),'\n',sep = '')
            
            cat('\n',blue('metabolyseR '),bold(red(str_c('v',object@log$packageVersion %>% as.character()))),yellow('\nAnalysis:\n'),'\t',time,'\n',sep = '')
            cat('\n\tRaw Data:\n',rD,sep = '')
            
            if ('preTreated' %in% elements) {
              time <- object@log$preTreatment
              pD <- preTreatedData(object)
              pI <- preTreatedData(object)
              pD <- paste('\t\tNo. samples = ',nrow(pI),'\n','\t\tNo. variables = ',ncol(pD),'\n',sep = '')
              
              cat('\n\tPre-treated Data:\n','\t\t',time,'\n',pD,sep = '')
            }
            
            if ('modelling' %in% elements) {
              time <- object@log$modelling
              mR <- modellingResults(object)
              mR <- str_c('Methods: ',str_c(names(mR),collapse = ','))
              
              cat('\n\tModelling:\n','\t\t',time,'\n','\t\t',mR,'\n',sep = '')
            }
            
            if ('correlations' %in% elements) {
              time <- object@log$correlations
              corR <- correlationResults(object)
              corR <- paste('\t\tNo. correlations = ',nrow(corR),'\n',sep = '')
              cat('\n\tCorrelations:\n','\t\t',time,'\n',corR,sep = '')
            }
            
          }
)

#' show-AnalysisData
#' @description show method for AnalysisData class
#' @param object S4 object of class AnalysisData
#' @export

setMethod('show',signature = 'AnalysisData',
          function(object){
            cat('\nAnalysis Data object containing:\n\n')
            cat('Samples:',nrow(dat(object)),'\n')
            cat('Features:',ncol(dat(object)),'\n')
            cat('Info:',ncol(sinfo(object)),'\n')
            cat('\n')
          }
)

#' show-RandomForest
#' @description Show method for RandomForest class.
#' @param object S4 object of class RandomForest
#' @export

setMethod('show',signature = 'RandomForest',
          function(object){
            if (object@type != 'unsupervised') {
              cat('\nRandom forest',object@type,'\n\n')  
            } else {
              cat('\nUnsupervised random forest\n\n')
            }
            
            cat('Samples:\t',nrow(dat(object@data)),'\n')
            cat('Features:\t',ncol(dat(object@data)),'\n')
            
            if (object@type != 'unsupervised') {
              cat('Response:\t',measures(object) %>%
                    .$Response %>%
                    unique() %>%
                    str_c(collapse = ', '),'\n')  
            }
            
            if (object@type == 'classification') {
              cat('# comparisons:\t',measures(object) %>%
                    .$Comparison %>%
                    unique() %>%
                    length(),'\n')
            }
            
            cat('\n')
          })

#' show-Univariate
#' @description Show method for the Univariate class.
#' @param object S4 object of class Univariate
#' @export

setMethod('show',signature = 'Univariate',
          function(object){
            cat('\nUnivariate',object@type,'analysis\n\n')  
            
            cat('Samples:\t',nrow(dat(object@data)),'\n')
            cat('Features:\t',ncol(dat(object@data)),'\n')
            cat('Responses:\t',importance(object) %>%
                  .$Response %>%
                  unique() %>%
                  str_c(collapse = ', '),'\n')  
            if (object@type != 'linear regression') {
              cat('# comparisons:\t',importance(object) %>%
                    .$Comparison %>%
                    unique() %>%
                    length(),'\n')
              
            }
            cat('\n')
          })