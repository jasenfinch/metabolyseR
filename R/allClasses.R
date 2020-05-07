#' AnalysisData
#' @rdname AnalysisData-class
#' @description An S4 class for sample data and info.
#' @slot data sample data
#' @slot info sample info
#' @export

setClass('AnalysisData',
         slots = list(
           data = 'tbl_df',
           info = 'tbl_df'
         ),
         prototype = list(data = tibble(),info = tibble())
)

#' AnalysisParameters
#' @rdname AnalysisParameters-class
#' @description An S4 class to store analysis parameters
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

#' Analysis
#' @rdname Analysis-class
#' @description An S4 class to store analysis results
#' @slot log list containing analysis dates and time
#' @slot parameters class AnalysisParameters containing the analysis parameters
#' @slot rawData list containing info and raw data
#' @slot preTreated list containing preTreated info and raw data 
#' @slot modelling list containing modelling results
#' @slot correlations tibble containing weighted edgelist of correlations
#'@export

setClass('Analysis',
         slots = list(
           log = 'list',
           parameters = 'AnalysisParameters',
           rawData = 'AnalysisData',
           preTreated = 'AnalysisData',
           modelling = 'list',
           correlations = 'tbl_df'
         )
)

#' RandomForest
#' @rdname RandomForest-class
#' @description An S4 class for random forest results and models
#' @slot type random forest type
#' @slot data AnalysisData object of data used for modelling
#' @slot results list of measure and importance results tables
#' @slot predictions tibble of model observation predictions
#' @slot permutations list of permutations measure and importance results tables
#' @slot importances tibble of model feature importances
#' @slot proximities tibble of model observation proximities
#' @slot models list of random forest models
#' @export

setClass('RandomForest',
         slots = list(
           type = 'character',
           data = 'AnalysisData',
           results = 'list',
           predictions = 'tbl_df',
           permutations = 'list',
           importances = 'tbl_df',
           proximities = 'tbl_df',
           models = 'list'
         )
)

#' Univariate
#' @rdname Univariate-class
#' @description An S4 class for univariate test models and results.
#' @slot type univariate test type
#' @slot data AnalysisData object of tested data
#' @slot models list of model objects
#' @slot results tibble containing test results 
#' @export

setClass('Univariate',
         slots = list(
           type = 'character',
           data = 'AnalysisData',
           models = 'list',
           results = 'tbl_df'
         ))