#' Create an `AnalysisParameters` S4 class object
#' @description Initiate an `AnalysisParameters` object with the default analysis parameters for each of the analysis elements.
#' @param elements character vector containing elements for analysis. 
#' @return An S4 object of class `AnalysisParameters` containing the default analysis parameters.
#' @examples 
#' p <- analysisParameters()
#' 
#' print(p)
#' @importFrom methods new
#' @export

analysisParameters <- function(elements = analysisElements()){
  
  if (!is.character(elements)) {
    stop(
      'Argument "elements" should be a character vector.',
      call. = FALSE)
  }
  
  if (FALSE %in% (elements %in% analysisElements())) {
    elements <- analysisElements() %>%
      str_c('"',.,'"')
    stop(
      str_c('Vector elements of argument "elements" should be one of ',
            str_c(elements,collapse = ', '),
            '.'),
      call. = FALSE)
  }
  
  preTreat <- list()
  modelling <- list()
  correlations <- list()
  
  if ('pre-treatment' %in% elements) {
    preTreat <- list(
      QC = list(
        occupancyFilter = as.list(
          formals(QCMethods('occupancyFilter'))[-1]),
        impute = as.list(formals(QCMethods('impute'))[-1]),
        RSDfilter = as.list(formals(QCMethods('RSDfilter'))[-1]),
        removeQC = as.list(formals(QCMethods('removeQC'))[-1])
      ), 
      occupancyFilter = list(
        maximum = as.list(formals(occupancyMethods('maximum'))[-1])),
      impute = list(class = as.list(formals(imputeMethods('class'))[-1])),
      transform = list(
        TICnorm = as.list(formals(transformMethods('TICnorm'))[-1]))
    )
  }
  
  if ('modelling' %in% elements) {
    modelling <- modellingParameters('randomForest')
  }
  if ('correlations' %in% elements) {
    correlations <- list(
      method = 'pearson',
      pAdjustMethod = 'bonferroni',
      corPvalue = 0.05,
      minCoef = 0,
      maxCor = Inf
    )
  }
  
  p <- new('AnalysisParameters',
           `pre-treatment` = preTreat,
           modelling = modelling,
           correlations = correlations
  )
  
  return(p)
}

#' Analysis elements
#' @description Return the analysis elements available in `metabolyseR`.
#' @return A character vector of analysis elements.
#' @examples 
#' analysisElements()
#' @export

analysisElements <- function(){
  new('AnalysisParameters') %>%
    slotNames()
}


#' Get or set analysis parameters
#' @rdname parameters
#' @description Get or set parameters for `AnalysisParameters` or `Analysis` class objects.
#' @param d S4 object of class `AnalysisParameters` or `Analysis`
#' @param element analysis element for parameters to extract or assign. 
#' Should be one of those returned by \code{analysisElements()}
#' @param value list containing parameter values
#' @param ... arguments to pass to the appropriate method
#' @examples 
#' p <- analysisParameters('pre-treatment')
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
#' 
#' print(p)
#' @export

setGeneric('parameters',function(d,...)
  standardGeneric('parameters'))

#' @rdname parameters

setMethod('parameters',signature = 'AnalysisParameters',
          function(d,element){
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(
                str_c('Argument "element" should be one of ',
                      str_c(elements,collapse = ', '),'.'),
                call. = FALSE)
            }
            slot(d,element)
          }
)

#' @rdname parameters

setMethod('parameters',signature = 'Analysis',
          function(d){
            d@parameters
          }
)

#' @rdname parameters
#' @export

setGeneric('parameters<-',function(d,element,value)
  standardGeneric('parameters<-'))

#' @rdname parameters

setMethod('parameters<-',signature = 'AnalysisParameters',
          function(d,element,value){
            
            if (!(element %in% analysisElements())) {
              elements <- analysisElements() %>%
                str_c('"',.,'"')
              stop(
                str_c('Argument "element" should be one of ',
                      str_c(elements,collapse = ', ')
                      ,'.'),
                call. = FALSE)
            }
            
            checkParameters(value,element)
            
            slot(d,element) <- value
            return(d)
          }
)

#' @rdname parameters

setMethod('parameters<-',signature = 'Analysis',
          function(d,value){
            d@parameters <- value
            return(d)
          }
)

checkPreTreatmentParameters <- function(value){
  
  if (FALSE %in% (names(value) %in% preTreatmentElements())) {
    elements <- preTreatmentElements() %>%
      str_c('"',.,'"')
    stop(
      str_c('List names for replacement value can only include ',
            str_c(elements,collapse = ', '),
            '.'),
      call. = FALSE)
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
        stop(
          str_c('Methods for element "',.x,'" can only include ',
                str_c(all_methods,collapse = ', '),'.'),
          call. = FALSE)
      }
    })
}

checkModellingParameters <- function(value){
  if (FALSE %in% (names(value) %in%(modellingMethods()))) {
    methods <- modellingMethods() %>% 
      str_c('"',.,'"')
    stop(str_c('List modelling methods names for replacement can only include ',
         str_c(methods,collapse = ', '),'.'),
         call. = FALSE)
  }
  
  value %>%
    names() %>%
    walk(~{
      all_parameters <- .x %>%
        modellingParameters() %>% 
        .[[1]] %>% 
        names()
      parameters <- value[[.x]] %>%
        names()
      if (FALSE %in% (parameters %in% all_parameters)) {
        all_parameters <- all_parameters %>%
          str_c('"',.,'"')
        stop(
          str_c('Parameters for method "',.x,'" can only include ',
                str_c(all_parameters,collapse = ', '),'.'),
          call. = FALSE)
      }
    })
}

checkCorrelationParameters <- function(value){
  if (FALSE %in% (names(value) %in% names(correlationsParameters()))) {
    p <- names(correlationsParameters()) %>%
      str_c('"',.,'"')
    stop(
      str_c('Correlation parameter values should match',
            str_c(p,collapse = ', '),
            '.'),
      call. = FALSE)
  }
  
  if ('method' %in% names(value)) {
    methods <- eval(formals(rcorr)$type)
    if (!(value$method %in% methods)) {
      methods <- str_c('"',methods,'"')
      stop(
        str_c('The value of parameter "method" should be one of ',
              str_c(methods,collapse = ', '),'.'),
        call. = FALSE)
    } 
  }
  
  if ('pAdjustMethod' %in% names(value)) {
    if (!(value$pAdjustMethod %in% p.adjust.methods)) {
      methods <- str_c('"',p.adjust.methods,'"')
      stop(
        str_c('The value of parameter "pAdjustMethod" should be one of ',
              str_c(methods,collapse = ', '),'.'),
        call. = FALSE)
    } 
  }
  
  if ('corPvalue' %in% names(value)) {
    if (!is.numeric(value$corPvalue) | length(value$corPvalue) > 1) {
      stop(
        'The value of parameter "corPvalue" should be a single numeric value.',
        call. = FALSE)
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

#' Change analysis parameters
#' @rdname changeParameter
#' @description Change analysis parameters.
#' @param x S4 object of class `AnalysisParameters`
#' @param parameterName name of the parameter to change
#' @param elements character vector of analysis elements to target parameter 
#' change. Can be any returned by `analysisElements()`.
#' @param value New value of the parameter
#' @return An S4 object of class `AnalysisParameters`.
#' @details
#' For the parameter name selected, all parameters with that name will 
#' be altered. 
#' @examples 
#' p <- analysisParameters('pre-treatment')
#' 
#' changeParameter(p,'cls') <- 'day'
#' 
#' print(p)
#' @export

setGeneric("changeParameter<-", 
           function(
             x,
             parameterName,
             elements = analysisElements(), 
             value) 
             standardGeneric("changeParameter<-"))

#' @rdname changeParameter
#' @importFrom purrr map_lgl
#' @importFrom stats p.adjust.methods

setMethod('changeParameter<-',signature = 'AnalysisParameters',
          function(x,parameterName,elements = analysisElements(), value) {
            
            ele <- analysisElements()
            
            if (FALSE %in% (map_lgl(elements,~{. %in% ele}))) {
              e <- str_c('"',ele,'"')
              stop(str_c('Elements can only include ',str_c(e,collapse = ', ')))
            }
            
            elements <- elements[map_dbl(elements,~{
              length(slot(x,.x))
            }) > 0]
            
            
            if ('pre-treatment' %in% elements) {
              parameters(x,'pre-treatment') <- x %>%
                parameters('pre-treatment') %>%
                map(~{
                  .x %>%
                    map(~{
                      if (parameterName %in% names(.x)) {
                        .x[[parameterName]] <- value
                      }
                      return(.x)
                    })
                })
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

#' Parse/export analysis parameters
#' @rdname io-parameters
#' @description Import analysis parameters from a `.yaml` format file or export an `AnalysisParameters` object to `.yaml` format.
#' @param d S4 object of class AnalysisParameters or Analysis
#' @param path file path of .yaml file to parse
#' @param file File name and path to export to
#' @examples 
#' ## Import analysis parameters
#' paramFile <- system.file('defaultParameters.yaml',package = 'metabolyseR')
#' p <- parseParameters(paramFile)
#' p
#' 
#' \dontrun{
#' ## Export analysis parameters
#' exportParameters(p,file = 'analysis_parameters.yaml')
#' }
#' @importFrom yaml read_yaml
#' @importFrom stringr str_remove_all
#' @export

parseParameters <- function(path){
  par <- read_yaml(path)
  
  par <- par %>%
    map(~{
      names(.) <- str_remove_all(names(.),'[\\b\\d+\\b]')
      return(.)
    })
  
  if ('pre-treatment' %in% names(par)) {
    par$preTreat <- par$preTreat %>%
      map(~{
        .x %>%
          map(~{
            .x %>%
              map(~{
                if (is.list(.x)) {
                  .x <- unlist(.x,use.names = FALSE)
                }
                return(.x)
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

#' @rdname io-parameters
#' @export

setGeneric('exportParameters',function(d,file = 'analysis_parameters.yaml')
  standardGeneric('exportParameters'))

#' @rdname io-parameters
#' @importFrom yaml write_yaml

setMethod('exportParameters',signature = 'AnalysisParameters',
          function(d,file = 'analysis_parameters.yaml'){
            elements <- analysisElements()
            
            p <- elements %>%
              map(~{
                parameters(d,.x)
              }) %>%
              set_names(elements)
            
            ele_len <- map_dbl(p,length)
            
            p <- p[ele_len > 0]
            
            p %>%
              names() %>%
              map(~{
                params <- p[[.x]]
                
                if (.x == 'pre-treatment') {
                  params <- params %>%
                    map(~{
                      .x <- .x %>%
                        map(~{
                          .x %>%
                            map(eval)
                        })
                      return(.x)
                    })
                }
                
                if (.x == 'modelling') {
                  params <- params %>%
                    map(~{
                      .x <- .x %>%
                        map(eval)
                      return(.x)
                    })
                }
                
                return(params)
              }) %>%
              set_names(names(p)) %>%
              write_yaml(file)
          }
)

#' @rdname io-parameters 
#' @export

setMethod('exportParameters',signature = 'Analysis',
          function(d,file = 'analysis_parameters.yaml'){
            d %>%
              parameters() %>%
              exportParameters(file = file)
          }
)

#' Pre-treatment parameters
#' @rdname pre-treatment-parameters
#' @description Return pre-treatment elements, methods and parameters.
#' @param element pre-treatment element name
#' @param methods a named list of element methods
#' @examples
#' ## Return the availalble pre-treatment elements
#' preTreatmentElements()
#' 
#' ## Return the available pre-treatment methods for the remove element
#' preTreatmentMethods('remove')
#' 
#' ## Define some default pre-treatment parameters
#' p <- preTreatmentParameters(
#'   list(
#'     remove = 'classes',
#'     QC = c('RSDfilter','removeQC'),
#'     transform = 'TICnorm'
#'   )
#' )
#' 
#' ## Assign the pre-treatment parameters to analysis parameters
#' ap <- analysisParameters('pre-treatment')
#' parameters(ap,'pre-treatment') <- p
#' 
#' print(ap)
#' @importFrom purrr walk
#' @export

preTreatmentElements <- function(){
  getPreTreatMethods() %>%
    names()
}

#' @rdname pre-treatment-parameters
#' @export

preTreatmentMethods <- function(element){
  getPreTreatMethods(element)() %>%
    names() %>%
    sort()
}

#' @rdname pre-treatment-parameters
#' @export

preTreatmentParameters <- function(methods){
  
  if (!is.list(methods)) {
    stop(
      'Argument "methods" should be a named list of element methods.',
      call. = FALSE)
  }
  
  if (FALSE %in% (names(methods) %in% preTreatmentElements())) {
    elements <- preTreatmentElements() %>%
      str_c('"',.,'"')
    stop(
      str_c('List names of argument "methods" can only include ',
            str_c(elements,collapse = ', '),
            '.'),
      call. = FALSE)
  }
  
  methods %>%
    names() %>%
    walk(~{
      meths <- .x %>%
        preTreatmentMethods()
      if (FALSE %in% (methods[[.x]] %in% meths)) {
        meths <- meths %>%
          str_c('"',.,'"')
        stop(
          str_c('Methods for element "',
                .x,'" can only include ',
                str_c(meths,collapse = ', '),
                '.'),
          call. = FALSE)
      }
    })
  
  methods %>%
    length() %>%
    {
      seq_len(.) 
    } %>%
    map(~{
      m <- methods %>%
        names() %>%
        .[.x]
      element <- m %>%
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

#' Modelling parameters
#' @rdname modelling-parameters
#' @description Retrieve the available modelling methods and parameters.
#' @param methods character vector of available modelling methods
#' @examples 
#' ## Retrieve the available modelling methods
#' modellingMethods()
#' 
#' ## Retrieve the modelling parameters for the anova method
#' p <- modellingParameters('anova')
#' 
#' ## Assign the modelling parameters to analysis parameters
#' mp <- analysisParameters('modelling')
#' 
#' parameters(mp,'modelling') <- p
#' 
#' print(mp)
#' @export

modellingMethods <- function(){
  getModellingMethods() %>%
    names()
}

#' @rdname modelling-parameters
#' @export

modellingParameters <- function(methods){
  
  if (!is.character(methods)) {
    stop(
      'Argument "methods" should be a character vector.',
      call. = FALSE)
  }
  
  if (FALSE %in% (methods %in% modellingMethods())) {
    stop(
      str_c('Modelling method not found! Methods should be one of: ',
            str_c(modellingMethods(),collapse = ', '),
            '.'))
  }
  
  methods %>%
    map(~{
      formals(.) %>%
        .[-1]
    }) %>%
    set_names(methods)
}

#' Correlations parameters
#' @description Retrieve the default parameters for correlation analysis.
#' @examples 
#' ## Retrieve the default correlation parameters
#' p <- correlationsParameters()
#' 
#' ## Assign the correlation parameters to analysis parameters
#' cp <- analysisParameters('correlations')
#' parameters(cp,'correlations') <- p
#' 
#' print(cp)
#' @export

correlationsParameters <- function(){
  doCorrelations %>%
    formals() %>%
    .[-1]
}
