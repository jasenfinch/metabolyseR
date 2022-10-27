#' @importFrom methods show
#' @importFrom purrr map_dbl map_chr

setMethod('show',signature = 'AnalysisParameters',
          function(object){
            
            elements <- analysisElements()
            elements <- elements[map_lgl(
              elements,
              ~{length(slot(object,.x)) != 0}) == TRUE]
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
              `pre-treatment` <- paste('\t',
                                       names(`pre-treatment`),
                                       '\n',
                                       `pre-treatment`,
                                       sep = '')
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
              modelling <- paste('\t',
                                 names(modelling),
                                 '\n',
                                 modelling,
                                 sep = '')
              modelling <- paste(modelling,collapse = '')
            }
            
            if ('correlations' %in% elements) {
              correlations <- slot(object,'correlations')
              correlations <- paste(names(correlations), 
                                    correlations,sep = ' = ')
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

#' @importFrom methods show
#' @importFrom crayon blue bold red

setMethod('show',signature = 'Analysis',
          function(object){
            elements <- slotNames(object)
            elements <- elements[4:length(elements)]
            elements <- elements[map_lgl(
              elements, 
              ~{length(slot(object,.x)) != 0}) == TRUE]
            names(elements) <- elements
            
            time <- object@log$analysis
            
            r <- raw(object)
            
            rD <- paste('\t\tNo. samples = ',
                        nSamples(r),
                        '\n',
                        '\t\tNo. features = ',
                        nFeatures(r),
                        '\n',
                        sep = '')
            
            cat('\n',
                blue('metabolyseR '),
                bold(red(str_c('v',object@log$packageVersion %>% 
                                 as.character()))),
                yellow('\nAnalysis:\n'),
                '\t',
                time,
                '\n',
                sep = '')
            cat('\n\tRaw Data:\n',rD,sep = '')
            
            if ('pre-treated' %in% elements) {
              time <- object@log$preTreatment
              p <- preTreated(object)
              
              pD <- paste('\t\tNo. samples = ',
                          nSamples(p),
                          '\n',
                          '\t\tNo. features = ',
                          nFeatures(p),
                          '\n',
                          sep = '')
              
              cat('\n\tPre-treated Data:\n','\t\t',time,'\n',pD,sep = '')
            }
            
            if ('modelling' %in% elements) {
              time <- object@log$modelling
              mR <- analysisResults(object,element = 'modelling')
              mR <- str_c('Methods: ',str_c(names(mR),collapse = ','))
              
              cat('\n\tModelling:\n','\t\t',time,'\n','\t\t',mR,'\n',sep = '')
            }
            
            if ('correlations' %in% elements) {
              time <- object@log$correlations
              corR <- analysisResults(object,element = 'correlations')
              corR <- paste('\t\tNo. correlations = ',nrow(corR),'\n',sep = '')
              cat('\n\tCorrelations:\n','\t\t',time,'\n',corR,sep = '')
            }
            
          }
)

setMethod('show',signature = 'AnalysisData',
          function(object){
            cat('\nAnalysisData object containing:\n\n')
            cat('Samples:',nrow(dat(object)),'\n')
            cat('Features:',ncol(dat(object)),'\n')
            cat('Info:',ncol(sinfo(object)),'\n')
            cat('\n')
          }
)

setMethod('show',signature = 'RandomForest',
          function(object){
            if (object@type != 'unsupervised') {
              cat('\nRandom forest',object@type,'\n\n')  
            } else {
              cat('\nUnsupervised random forest\n\n')
            }
            
            cat('Samples:\t',nSamples(object),'\n')
            cat('Features:\t',nFeatures(object),'\n')
            
            if (object@type != 'unsupervised') {
              cat('Response:\t',metrics(object) %>%
                    .$response %>%
                    unique() %>%
                    str_c(collapse = ', '),'\n')  
            }
            
            if (object@type == 'classification') {
              cat('# comparisons:\t',metrics(object) %>%
                    .$comparison %>%
                    unique() %>%
                    length(),'\n')
            }
            
            cat('\n')
          })

setMethod('show',signature = 'Univariate',
          function(object){
            cat('\nUnivariate',object@type,'analysis\n\n')  
            
            if (nSamples(object) > 0){
              cat('Samples:\t',nSamples(object),'\n')
              cat('Features:\t',nFeatures(object),'\n')
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
            }
            
            cat('\n')
          })
