setOldClass('gg')

#' AnalysisParameters
#' @rdname AnalysisParameters-class
#' @description An S4 class to store analysis parameters
#' @slot preTreat list containing parameters for data pre-treatment
#' @slot classification list containing parameters for classification
#' @slot featureSelection list containing parameters for feature selection
#' @slot correlations list containing parameters for correlations
#'@export

setClass('AnalysisParameters',
         slots = list(
           preTreat = 'list',
           classification = 'list',
           featureSelection = 'list',
           correlations = 'list'
         ))

#' Analysis
#' @rdname Analysis-class
#' @description An S4 class to store analysis results
#' @slot log list containing analysis dates and time
#' @slot parameters class AnalysisParameters containing the analysis parameters
#' @slot rawData list containing info and raw data
#' @slot preTreated list containing preTreated info and raw data 
#' @slot classification list containing classification results
#' @slot featureSelection list contain feature selection results for each method
#' @slot correlations tibble containing weighted edgelist of correlations
#'@export

setClass('Analysis',
         slots = list(
           log = 'list',
           parameters = 'AnalysisParameters',
           rawData = 'list',
           preTreated = 'list',
           classification = 'tbl_df',
           featureSelection = 'tbl_df',
           correlations = 'tbl_df'
         )
)

#' AnalysisPlot
#' @rdname AnalysisPlot-class
#' @description An S4 class to store an analysis plot
#' @slot data list containing data used to generate the plot
#' @slot plot ggplot object containing the plot
#' @slot func function used to generate the plot
#' @export

setClass('AnalysisPlot',
         slots = list(
           data = 'list',
           plot = 'gg',
           func = 'function'
         )
)