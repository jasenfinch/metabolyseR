#' Tune random forest parameters
#' @rdname tune
#' @description Tune the `mtry` and `ntree` random forest parameters using a grid search approach.
#' @param x S4 object of class `AnalysisData`
#' @param cls sample information column to use
#' @param mtry_range numeric vector of `mtry` values to search
#' @param ntree_range numeric vector of `ntree` values to search
#' @param seed random number seed
#' @details 
#' Parameter tuning is performed by grid search of all combinations of the `mtry_range` and `ntree_range` vectors provided.
#' The optimal parameter values are selected using the out-of-bag error estimates of the `margin` metric for classification and the `rmse` (root-mean-square error) metric for regression.
#' @return 
#' A list containing the optimal `mtry` and `ntree` parameters. 
#' This is suitable for use as the `rf` argument in method `randomForest()`.  
#' @examples
#' library(metaboData)
#' 
#' ## Prepare some data
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'   occupancyMaximum(cls = 'day') %>%
#'   transformTICnorm()
#'
#' ## Tune the `mtry` parameter for the `day` response
#' tune(x,cls = 'day')
#' @export

setGeneric("tune", function(x,
                            cls = 'class',
                            mtry_range = floor(seq(mtry(x,cls = cls) - mtry(x,cls = cls)/2,
                                                   mtry(x,cls = cls) + mtry(x,cls = cls)/2,
                                                   length.out = 4)),
                            ntree_range = 1000,
                            seed = 1234)
  standardGeneric("tune"))

#' @rdname tune
#' @importFrom tidyr expand_grid
#' @importFrom dplyr rename_with
#' @importFrom stringr str_remove
#' @importFrom furrr future_map2

setMethod('tune',signature = 'AnalysisData',
          function(x,
                   cls = 'class',
                   mtry_range = floor(seq(mtry(x,cls = cls) - mtry(x,cls = cls)/2,
                                          mtry(x,cls = cls) + mtry(x,cls = cls)/2,
                                          length.out = 4)),
                   ntree_range = 1000,
                   seed = 1234){
            
            if (is.null(cls)){
              stop("Can't tune unsupervised random forest.",
                   call. = FALSE)
            }
            
            response <- clsExtract(x,cls = cls)
            
            rf_type <- ifelse(is.numeric(response),
                              'regression',
                              'classification')
            
            metric <- switch(rf_type,
                             regression = 'rmse',
                             classification = 'margin')
            
            combinations <- expand_grid(mtry_range,
                                        ntree_range) %>%
              rename_with(~ str_remove(.x,
                                       '_range'))
            
            search_results <- combinations %>%
              {
                future_map2(
                  .$ntree,
                  .$mtry,
                  .f = ~{
                    rf_res <- try(randomForest(x,
                                               cls = cls,
                                               rf = list(ntree = .x,
                                                         mtry = .y)),
                                  silent = TRUE)
                    if (class(rf_res) == 'RandomForest'){
                      rf_res %>% 
                        metrics() %>% 
                        select(-response,-.estimator,-contains('comparison')) %>% 
                        spread(.metric,.estimate) %>% 
                        mutate(ntree = .x,
                               mtry = .y)
                    } else {
                      NULL
                    }
                    
                  },
                  .options = furrr_options(seed = seed)) 
              } %>%
              bind_rows() 
            
            if (nrow(search_results) > 0){
              search_results <- switch(metric,
                                       rmse = search_results %>%
                                         arrange(!!sym(metric)) ,
                                       margin = search_results %>%
                                         arrange(desc(!!sym(metric)))) %>% 
                {list(mtry = .$mtry[1],
                      ntree = .$ntree[1])}  
              
              return(search_results)
            } else {
              return(list())
            }
            
          })
