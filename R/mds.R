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
                                 classification = c('response','comparison'),
                                 regression = 'response',
                                 unsupervised = NULL)
            
            dissimilarities <- x %>% 
              proximity(idx = idx) %>% 
              mutate(across(sample1:sample2,as.character)) %>% 
              spread(sample2,proximity) %>% 
              mutate_if(is.numeric,~ 1 - .x)
            
            mds_dimensions <- dissimilarities %>% 
              group_by_at(group_vars) %>% 
              group_map(~ .x %>% 
                          select_if(
                            ~is.numeric(.x) & !any(is.na(.x)) 
                          ) %>% 
                          cmdscale(k = dimensions) %>% 
                          set_colnames(str_c('dimension ',
                                             seq_len(dimensions))) %>% 
                          as_tibble() %>% 
                          bind_cols(select_if(.x %>% 
                                                ungroup(),
                                              is.character)) %>%
                          relocate(contains('dimension'),
                                   .after = last_col()),
                        .keep = TRUE
              ) %>% 
              bind_rows() %>% 
              rename(sample = sample1)
            
            if (is.null(idx)){
              mds_dimensions <- mds_dimensions %>% 
                mutate(sample = as.numeric(sample)) %>% 
                arrange(across(all_of(c(group_vars,'sample'))))
            }
            
            return(mds_dimensions)
          }
)

#' @rdname mds

setMethod('mds',signature = 'list',
          function(x,dimensions = 2,idx = NULL){
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
                map(mds,dimensions = dimensions,idx = idx) %>%
                bind_rows()  
            } else {
              tibble()
            }
            
          })

#' @rdname mds

setMethod('mds',signature = 'Analysis',
          function(x,dimensions = 2,idx = NULL){
            x %>% 
              analysisResults('modelling') %>% 
              mds(dimensions = dimensions,
                  idx = idx)
          })