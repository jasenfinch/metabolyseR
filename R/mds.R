#' Multidimensional scaling (MDS)
#' @rdname mds
#' @description Multidimensional scaling of random forest proximities.
#' @param x S4 object of class `RandomForest`, `Analysis` or a list
#' @param dimensions The number of dimensions by which the data are to be represented.
#' @param idx sample information column to use for sample names. If `NULL`, the sample row number will be used. Sample names should be unique for each row of data.
#' @return 
#' A tibble containing the scaled dimensions.
#' @examples
#' library(metaboData)
#' 
#' x <- analysisData(abr1$neg[,200:300],abr1$fact) %>%
#'        occupancyMaximum(cls = 'day') %>%
#'        transformTICnorm()
#'        
#' rf <- randomForest(x,cls = 'day')
#' 
#' mds(rf)
#' @export

setGeneric("mds", function(x,dimensions = 2,idx = NULL) 
  standardGeneric("mds")
)

#' @rdname mds
#' @importFrom dplyr mutate_if group_map select_if across

setMethod('mds',signature = 'RandomForest',
          function(x,dimensions = 2,idx = NULL){
            
            group_vars <- switch(type(x),
                                 classification = c('Response','Comparison'),
                                 regression = 'Response',
                                 unsupervised = NULL)
            
            dissimilarities <- x %>% 
              proximity(idx = idx) %>% 
              mutate(across(Sample1:Sample2,as.character)) %>% 
              spread(Sample2,Proximity) %>% 
              mutate_if(is.numeric,~ 1 - .x)
            
            mds_dimensions <- dissimilarities %>% 
              group_by_at(group_vars) %>% 
              group_map(~ .x %>% 
                          select_if(is.numeric) %>% 
                          cmdscale(k = dimensions) %>% 
                          set_colnames(str_c('Dimension ',
                                             seq_len(dimensions))) %>% 
                          as_tibble() %>% 
                          bind_cols(select_if(.x %>% 
                                                ungroup(),
                                              is.character)) %>%
                          relocate(contains('Dimension'),
                                   .after = last_col()),
                        .keep = TRUE
              ) %>% 
              bind_rows() %>% 
              rename(Sample = Sample1)
            
            if (is.null(idx)){
              mds_dimensions <- mds_dimensions %>% 
                mutate(Sample = as.numeric(Sample)) %>% 
                arrange(across(c(group_vars,'Sample')))
            }
            
            return(mds_dimensions)
          }
)

#' @rdname mds

setMethod('mds',signature = 'list',
          function(x,dimensions = 2){
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
                map(mds,dimensions = dimensions) %>%
                bind_rows()  
            } else {
              tibble()
            }
            
          })

#' @rdname mds

setMethod('mds',signature = 'Analysis',
          function(x,dimensions = 2){
            x %>% 
              analysisResults('modelling') %>% 
              mds()
          })