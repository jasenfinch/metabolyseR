#' Predict random forest model responses
#' @rdname predict
#' @description Predict values of random forest model response variables from new data.
#' @param model S4 object of class `RandomForest`
#' @param new_data S4 object of class `AnalysisData`
#' @param idx sample information column to use for sample names. If `NULL`, the sample row number will be used. Sample names should be unique for each row of data.
#' @param type one of `response`, `prob`, or `votes` to indicate the type of prediction to make
#' @param ... arguments to pass to `randomForest::predict.randomForest()`
#' @details
#' The features contained within `new_data` should match those of the features used to train `model`.
#' The `features()` method can be used to check this.
#' The argument `returnModels = TRUE` should also be used when training the `RandomForest-class` object used for argument `model`.
#' @examples 
#' library(metaboData)
#' 
#' ## Prepare some data
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'   occupancyMaximum(cls = 'day') %>%
#'   transformTICnorm()
#' 
#' ## Extract data from which to train a random forest model
#' training_data <- x %>% 
#'   keepClasses(cls = 'day',
#'               classes = c('H','1'))
#' 
#' ## Extract data for which response values will be predicted
#' test_data <- x %>% 
#'   keepClasses(cls = 'day',
#'               classes = c('2','3'))
#' 
#' rf <- randomForest(training_data,
#'                    cls = 'day',
#'                    returnModels = TRUE)
#' 
#' predict(rf,
#'         test_data)
#' @importFrom purrr map_depth
#' @export

setGeneric("predict", function(model,
                               new_data,
                               idx = NULL,
                               type = c('response','prob','votes'),
                               ...)
  standardGeneric("predict"))

#' @rdname predict

setMethod('predict',signature = c('RandomForest','AnalysisData'),
          function(model,
                   new_data,
                   idx = NULL,
                   type = c('response','prob','votes'),
                   ...){
            
            if (type(model) == 'unsupervised') {
              stop("Can't predict unsupervised random forest.",
                   call. = FALSE)
            }
            
            if(length(model@models) == 0){
              stop('No random forest models detected. Use argument `returnModels = TRUE` when running method `randomForest()`.',
                   call. = FALSE)
            }
            
            if (!is.null(idx)){
              sample_idx <- new_data %>% 
                clsExtract(cls = idx)
              
              if (any(duplicated(sample_idx))){
                stop(str_c('Duplicated sample names found in sample information column `',
                           idx,
                           '`. The specified sample names should be unique to each sample.'),
                     call. = FALSE)
              }
            } else {
              sample_idx <- seq_len(nSamples(new_data))
            }
            
            type <- match.arg(type,
                              c('response','prob','votes'))
            
            test_data <- dat(new_data)
            
            model_object_depth <- switch(type(model),
                                         classification = 3,
                                         regression = 2) 
            
            model_predictions <- model@models %>% 
              map_depth(.depth = model_object_depth,
                        .f = ~ .x %>% 
                          {
                            tibble(
                              sample = sample_idx,
                              prediction = stats::predict(
                                object = .x,
                                newdata = test_data,
                                type = type,
                                ...))
                          })
            
            column_headers <- c('response',
                                'comparison',
                                'rep')
            type_column_headers <- switch(
              type(model),
              classification = column_headers,
              regression = column_headers[c(1,3)]
            )
            
            for (i in rev(type_column_headers)) {
              model_predictions <- map_depth(.x = model_predictions,
                                             .depth = which(type_column_headers == i) - 1,
                                             .f = bind_rows,.id = i) 
            }
            
            model_predictions <- model_predictions %>% 
              mutate(rep = as.numeric(rep))
            
            return(model_predictions)
          })
