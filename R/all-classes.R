#' AnalysisData S4 class
#' @rdname AnalysisData-class
#' @description An S4 class for metabolomic data and sample meta information.
#' @slot data sample metabolomic data
#' @slot info sample meta information
#' @export

setClass('AnalysisData',
         slots = list(
           data = 'tbl_df',
           info = 'tbl_df'
         ),
         prototype = list(data = tibble(),info = tibble())
)

setValidity('AnalysisData',function(object){
  
  data <- dat(object)
  info <- sinfo(object)
  
  if (nrow(data) != nrow(info)) {
    'Number of rows in data should match number of rows in sample information!'
  } else {
    TRUE
  }
})

#' AnalysisParameters S4 class
#' @rdname AnalysisParameters-class
#' @description An S4 class to store analysis parameters.
#' @slot pre-treatment list containing parameters for data pre-treatment
#' @slot modelling list containing parameters for modelling
#' @slot correlations list containing parameters for correlations
#'@export

setClass('AnalysisParameters',
         slots = list(
           `pre-treatment` = 'list',
           modelling = 'list',
           correlations = 'list'
         ),
         prototype = list(
           `pre-treatment` = list(),
           modelling = list(),
           correlations = list()
         ))

#' Analysis S4 class
#' @rdname Analysis-class
#' @description An S4 class to store analysis results.
#' @slot log list containing analysis dates and time
#' @slot parameters class AnalysisParameters containing the analysis parameters
#' @slot raw list containing info and raw data
#' @slot pre-treated list containing preTreated info and raw data 
#' @slot modelling list containing modelling results
#' @slot correlations tibble containing weighted edgelist of correlations
#'@export

setClass('Analysis',
         slots = list(
           log = 'list',
           parameters = 'AnalysisParameters',
           raw = 'AnalysisData',
           `pre-treated` = 'AnalysisData',
           modelling = 'list',
           correlations = 'tbl_df'
         ),
         prototype = list(
           log = list(packageVersion =  packageVersion('metabolyseR') %>% 
                        as.character(),
                      analysis = date(),
                      verbose = TRUE)
         )
)

#' RandomForest S4 class
#' @rdname RandomForest-class
#' @description An S4 class for random forest results and models.
#' @slot type random forest type
#' @slot response response variable name
#' @slot metrics tibble of model performance metrics
#' @slot predictions tibble of model observation predictions
#' @slot permutations list of permutations measure and importance results tables
#' @slot importances tibble of model feature importances
#' @slot proximities tibble of model observation proximities
#' @slot models list of random forest models
#' @export

setClass('RandomForest',
         contains = 'AnalysisData',
         slots = list(
           type = 'character',
           response = 'character',
           metrics = 'tbl_df',
           predictions = 'tbl_df',
           permutations = 'list',
           importances = 'tbl_df',
           proximities = 'tbl_df',
           models = 'list'
         ),
         prototype = list(
           type = 'unsupervised',
           response = '',
           metrics = tibble(),
           predictions = tibble(),
           permutations = list(
             metrics = tibble(),
             importance = tibble()
           ),
           importances = tibble(),
           proximities = tibble(),
           models = list()
         )
)

#' Univariate S4 class
#' @rdname Univariate-class
#' @description An S4 class for univariate test models and results.
#' @slot type univariate test type
#' @slot models list of model objects
#' @slot results tibble containing test results 
#' @export

setClass('Univariate',
         contains = 'AnalysisData',
         slots = list(
           type = 'character',
           models = 'list',
           results = 'tbl_df'
         ))

setClass('LDA',
         contains = 'AnalysisData',
         slots = list(
           stats = 'tbl_df',
           Tw = 'numeric',
           rankmat = 'numeric',
           means = 'numeric',
           loadings = 'tbl_df',
           x = 'tbl_df',
           xmeans = 'tbl_df',
           pred = 'factor',
           cl = 'factor',
           prior = 'numeric',
           conf = 'table',
           acc = 'numeric',
           lev = 'character',
           call = 'call'
         ))