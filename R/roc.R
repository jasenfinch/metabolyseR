#' Receiver-operator characteristic (ROC) curves
#' @rdname roc
#' @description  ROC curves for out-of-bag random forest predictions.
#' @param x S4 object of class `RandomForest`, `Analysis` or a list
#' @param idx sample information column to use for sample names. If `NULL`, the sample row number will be used. Sample names should be unique for each row of data.
#' @return 
#' A tibble containing the ROC curves.
#' @examples
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' roc(rf)
#' @export

setGeneric("roc", function(x) 
  standardGeneric("roc")
)

#' @rdname roc

setMethod('roc',signature = 'RandomForest',
          function(x){
            
            if (type(x) != 'classification') {
              stop('ROC curves can only be plotted for classification!')
            }
            
            roc_curves <- x@predictions %>%
              group_by(Response,Comparison) %>% 
              group_map(~{
                .x <- .x %>% 
                  mutate(obs = factor(obs)) 
                
                if (length(levels(.x$obs)) > 2) {
                  .x %>%
                    group_by(Response,Comparison) %>% 
                    roc_curve(obs,levels(.x$obs))  
                } else {
                  .x %>%
                    group_by(Response,Comparison) %>% 
                    roc_curve(obs,levels(.x$obs)[1])  
                } 
              }, .keep = TRUE) %>%
              bind_rows() %>% 
              ungroup() 
            
            if ('.level' %in% colnames(roc_curves)) {
              roc_curves <- roc_curves %>% 
                rename(Class = .level)
            }
            
            return(roc_curves)
          })

#' @rdname roc

setMethod('roc',signature = 'list',
          function(x){
            object_classes <- x %>%
              map_chr(class)
            
            if (FALSE %in% (object_classes == 'RandomForest')) {
              message(
                str_c('All objects contained within supplied list ',
                      'that are not of class RandomForest will be ignored.'))
            }
            
            x <- x[object_classes == 'RandomForest']
            
            if (length(x) > 0) {
              x %>%
                map(roc) %>%
                bind_rows()  
            } else {
              tibble()
            }
            
          })

#' @rdname roc

setMethod('roc',signature = 'Analysis',
          function(x){
            x %>% 
              analysisResults('modelling') %>% 
              roc()
          })