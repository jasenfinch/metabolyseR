
#' analysisElements
#' @description Return the analysis elements available in metabolyseR.
#' @export

analysisElements <- function(){
  new('AnalysisParameters') %>%
    slotNames()
}

#' analysisParameters
#' @description Initiate default analysis parameters for analysis elements.
#' @param elements character vector containing elements for analysis. Default includes all available elements from \code{analysisElements}.
#' @importFrom parallel detectCores
#' @importFrom methods new
#' @export

analysisParameters <- function(elements = analysisElements()){
  
  if (!is.character(elements)) {
    stop('Argument "elements" should be a character vector.',call. = FALSE)
  }
  
  if (FALSE %in% (elements %in% analysisElements())) {
    elements <- analysisElements() %>%
      str_c('"',.,'"')
    stop(str_c('Vector elements of argument "elements" should be one of ',str_c(elements,collapse = ', '),'.'),call. = FALSE)
  }
  
  preTreat <- list()
  modelling <- list()
  correlations <- list()
  
  if ('pre-treatment' %in% elements) {
    preTreat <- list(QC = list(occupancyFilter = as.list(formals(QCMethods('occupancyFilter'))[-1]),
                               impute = as.list(formals(QCMethods('impute'))[-1]),
                               RSDfilter = as.list(formals(QCMethods('RSDfilter'))[-1]),
                               removeQC = as.list(formals(QCMethods('removeQC'))[-1])
    ), 
    occupancyFilter = list(maximum = as.list(formals(occupancyMethods('maximum'))[-1])),
    impute = list(class = as.list(formals(imputeMethods('class'))[-1])),
    transform = list(TICnorm = as.list(formals(transformMethods('TICnorm'))[-1]))
    )
  }
  
  if ('modelling' %in% elements) {
    modelling <- modellingParameters('randomForest')
  }
  if ('correlations' %in% elements) {
    correlations <- list(
      method = 'pearson',
      pAdjustMethod = 'bonferroni',
      corPvalue = 0.05
    )
  }
  
  p <- new('AnalysisParameters',
           `pre-treatment` = preTreat,
           modelling = modelling,
           correlations = correlations
  )
  
  return(p)
}


#' parameters
#' @rdname parameters
#' @description Extract parameters from an Analysis object or extract or set parameters in an AnalysisParameters object.
#' @param x S4 object of class Analysis
#' @examples 
#' p <- analysisParameters()
#' 
#' ## extract pre-treatment parameters
#' parameters(p,'pre-treatment')
#' 
#' ## set pre-treatment parameters
#' parameters(p,'pre-treatment') <- preTreatmentParameters(
#'   list(
#'     remove = 'classes',
#'     QC = c('RSDfilter','removeQC'),
#'     transform = 'TICnorm'
#'   )
#' )
#' @export

setMethod('parameters',signature = 'Analysis',
          function(x){
            x@parameters
          }
)

#' @rdname parameters
#' @export
setMethod('parameters<-',signature = 'Analysis',
          function(x,value){
            x@parameters <- value
            return(x)
          }
)

#' @rdname parameters
#' @export
setMethod('parameters',signature = 'AnalysisParameters',
          function(x,element){
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(str_c('Argument "element" should be one of ',str_c(elements,collapse = ', '),'.'),call. = FALSE)
            }
            slot(x,element)
          }
)

#' @rdname parameters
#' @export
setMethod('parameters<-',signature = 'AnalysisParameters',
          function(x,element,value){
            
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(str_c('Argument "element" should be one of ',str_c(elements,collapse = ', '),'.'),call. = FALSE)
            }
            
            checkParameters(value,element)
            
            slot(x,element) <- value
            return(x)
          }
)

checkPreTreatmentParameters <- function(value){
  
  if (FALSE %in% (names(value) %in% preTreatmentElements())) {
    elements <- preTreatmentElements() %>%
      str_c('"',.,'"')
    stop(str_c('List names of for replacement value can only include ',str_c(elements,collapse = ', '),'.'))
  }
  
  value %>%
    names() %>%
    walk(~{
      all_methods <- .x %>%
        preTreatmentMethods()
      methods <- value %>%
        .[[.x]] %>%
        names()
      if (FALSE %in% (methods %in% all_methods)) {
        all_methods <- all_methods %>%
          str_c('"',.,'"')
        stop(str_c('Methods for element "',.x,'" can only include ',str_c(all_methods,collapse = ', '),'.'),call. = FALSE)
      }
    })
}

checkModellingParameters <- function(value){
  
}

checkCorrelationParameters <- function(value){
  if (F %in% (names(value) %in% names(correlationsParameters()))) {
    p <- names(correlationsParameters()) %>%
      str_c('"',.,'"')
    stop(str_c('Correlation parameter values should match',str_c(p,collapse = ', '),'.'))
  }
  
  if ('method' %in% names(value)) {
    methods <- eval(formals(rcorr)$type)
    if (!(value$method %in% methods)) {
      methods <- str_c('"',methods,'"')
      stop(str_c('The value of parameter "method" should be one of ',str_c(methods,collapse = ', '),'.'),call. = FALSE)
    } 
  }
  
  if ('pAdjustMethod' %in% names(value)) {
    if (!(value$pAdjustMethod %in% p.adjust.methods)) {
      methods <- str_c('"',p.adjust.methods,'"')
      stop(str_c('The value of parameter "pAdjustMethod" should be one of ',str_c(methods,collapse = ', '),'.'),call. = FALSE)
    } 
  }
  
  if ('corPvalue' %in% names(value)) {
    if (!is.numeric(value$corPvalue) | length(value$corPvalue) > 1) {
      stop('The value of parameter "corPvalue" should be a single numeric value.',call. = FALSE)
    }  
  }
}

checkParameters <- function(value,element){
  
  if (!is.list(value)) {
    stop('Replacement value should be an object of class list.',call. = FALSE)
  }
  
  if (element == 'pre-treatment') {
    checkPreTreatmentParameters(value)
  }
  
  if (element == 'modelling') {
    checkModellingParameters(value)
  }
  
  if (element == 'correlations') {
    checkCorrelationParameters(value)
  }
}

#' changeParameter
#' @rdname changeParameter
#' @description change analysis parameters
#' @param parameterName Name of the parameter to change
#' @param parameters S4 object of class AnalysisParameters in which to change parameters
#' @param elements Character vector of analysis elements to target parameter change. Can be any returned by \code{analysisElements}.
#' @param value New value of the parameter
#' @details
#' For the parameter name selected, all parameters with that name will be altered.
#' To individually change identically named parameters use the \code{@} operator to access the appropriate slot directly.
#' @examples 
#' p <- analysisParameters()
#' changeParameter(p,'clusterType') <- 'PSOCK'
#' @importFrom purrr map_lgl
#' @export

setMethod('changeParameter<-',signature = 'AnalysisParameters',
          function(x,parameterName,elements = analysisElements(), value) {
            
            ele <- analysisElements()
            
            if (F %in% (map_lgl(elements,~{. %in% ele}))) {
              e <- str_c('"',ele,'"')
              stop(str_c('Elements can only include ',str_c(e,collapse = ', ')))
            }
            
            elements <- elements[sapply(elements,function(x,parameters){
              length(slot(parameters,x))
            },parameters = x) > 0]
            
            if ('pre-treatment' %in% elements) {
              pars <- lapply(parameters(x,'pre-treatment'),function(x,parameterName){
                x <- lapply(x,function(y,parameterName){names(y)[names(y) == parameterName]},parameterName = parameterName)
                x[sapply(x,length) == 0] <- NULL
                return(x)
              },parameterName = parameterName)
              pars[sapply(pars,length) == 0] <- NULL
              pars <- lapply(names(pars),function(x,pars){
                pars <- pars[[x]]
                pars <- lapply(names(pars),function(y,pars,n){
                  pars <- pars[[y]]
                  pars <- c(n,y,pars)
                  return(pars)
                },pars = pars,n = x)
                return(pars)
              },pars = pars)
              pars <- unlist(pars,recursive = F)
              
              if (!is.null(pars)) {
                p <- parameters(x,'pre-treatment')
                for (i in 1:length(pars)) {
                  p[[pars[[i]][1]]][[pars[[i]][2]]][[pars[[i]][3]]] <- value
                }
                parameters(x,'pre-treatment') <- p
              }
            }
            
            if ('modelling' %in% elements) {
              pars <- parameters(x,'modelling') %>%
                map(~{
                  p <- .
                  if (parameterName %in% names(p)) {
                    p[[parameterName]] <- value
                  }
                  return(p)
                })
              parameters(x,'modelling') <- pars
            }
            
            if ('correlations' %in% elements) {
              pars <- names(parameters(x,'correlations'))
              if (parameterName %in% pars) {
                parameters(x,'correlations')[[parameterName]] <- value
              }
            }
            
            return(x)
          })

#' preTreatmentParameters
#' @description Return default parameters for given pre-treatment element methods. 
#' @param methods a named list of element methods
#' @examples 
#' p <- preTreatmentParameters(
#'   list(
#'     remove = 'classes',
#'     QC = c('RSDfilter','removeQC'),
#'     transform = 'TICnorm'
#'   )
#' )
#' @importFrom purrr walk
#' @export

preTreatmentParameters <- function(methods){
  
  if (!is.list(methods)) {
    stop('Argument "methods" should be a named list of element methods.',call. = FALSE)
  }
  
  if (FALSE %in% (names(methods) %in% preTreatmentElements())) {
    elements <- preTreatmentElements() %>%
      str_c('"',.,'"')
    stop(str_c('List names of argument "methods" can only include ',str_c(elements,collapse = ', '),'.'))
  }
  
  methods %>%
    names() %>%
    walk(~{
      meths <- .x %>%
        preTreatmentMethods()
      if (FALSE %in% (methods[[.x]] %in% meths)) {
        meths <- meths %>%
          str_c('"',.,'"')
        stop(str_c('Methods for element "',.x,'" can only include ',str_c(meths,collapse = ', '),'.'),call. = FALSE)
      }
    })
  
  methods %>%
    names() %>%
    map(~{
      element <- .x %>%
        getPreTreatMethods()
      meths <- methods[[.x]] %>%
        map(~{
          .x %>%
            element() %>%
            formals() %>%
            .[-1]
        }) %>%
        set_names(methods[[.x]])
    }) %>%
    set_names(names(methods))
}

#' modellingParameters
#' @description Return default parameters for a given modelling method.
#' @param methods character vector of available methods. Use \code{modellingMethods()} to see available methods.
#' @examples 
#' p <- analysisParameters()
#' parameters(p,'modelling') <- modellingParameters('anova')
#' @export

modellingParameters <- function(methods){
  
  if (!is.character(methods)) {
    stop('Argument "methods" should be a character vector.',call. = FALSE)
  }
  
  if (F %in% (methods %in% modellingMethods())) {
    stop(str_c('Modelling method not found! Methods should be one of: ',str_c(modellingMethods(),collapse = ', '),'.'))
  }
  
  methods %>%
    map(~{
      formals(.) %>%
        .[-1]
    }) %>%
    set_names(methods)
}

#' correlationsParameters
#' @description Return default parameters for correlation analysis
#' @examples 
#' p <- analysisParameters()
#' parameters(p,'correlations') <- correlationsParameters()
#' @export

correlationsParameters <- function(){
  doCorrelations %>%
    formals() %>%
    .[-1]
}

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
