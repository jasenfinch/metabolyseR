#' occupancyMaximum
#' @rdname occupancyMaximum
#' @description Maximum occupancy filtering of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @param occupancy occupancy threshold
#' @export

setMethod('occupancyMaximum',signature = 'AnalysisData',
          function(dat,cls = 'class', occupancy = 2/3){
            occ <- occupancy(dat,cls)
            fd <- occ %>%
              group_by(Feature) %>%
              summarise(Occupancy = max(Occupancy)) %>%
              filter(Occupancy >= occupancy)
            feat <- colnames(dat %>% dat)[colnames(dat %>% dat) %in% unique(fd$Feature)]
            dat@data <- dat %>%
              dat %>%
              select(feat)
            return(dat)
          }
)

#' occupancyMinimum
#' @rdname occupancyMinimum
#' @description Minimum occupancy filtering of sample data.
#' @param dat S4 object of class Data
#' @param cls info column to use for class data
#' @param occupancy occupancy threshold
#' @export

setMethod('occupancyMinimum',signature = 'AnalysisData',
          function(dat,cls = 'class', occupancy = 2/3){
            occ <- occupancy(dat,cls)
            fd <- occ %>%
              group_by(Feature) %>%
              summarise(Occupancy = min(Occupancy)) %>%
              filter(Occupancy >= occupancy)
            feat <- colnames(dat %>% dat())[colnames(dat %>% dat()) %in% unique(fd$Feature)]
            dat@data <- dat@data %>%
              select(feat)
            return(dat)
          }
)

occupancyMethods <- function(method = NULL, description = FALSE){
  
  methods <- list(
    maximum = occupancyMaximum, 
    minimum = occupancyMinimum
  )
  
  descriptions <- list(
    maximum = list(description = 'maximum thresholded class occupancy filtering', 
                   arguments = c(cls = 'info column to use for class labels', 
                                 occupancy = 'occupancy threshold')),
    minimum = list(description = 'minimum thresholded class occupancy filtering', 
                   arguments = c(cls = 'info column to use for class labels', 
                                 occupancy = 'occupancy threshold'))
  )
  
  if (description == FALSE) {
    if (is.null(method)) {
      method <- methods
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Occupancy method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- methods[[method]]
    }
  } else {
    if (is.null(method)) {
      method <- descriptions
    } else {
      if (!(method %in% names(methods))) {
        stop(str_c("Occupancy method '",
                   method,
                   "' not recognised. Available methods include: ",
                   str_c(str_c("'",names(methods),"'"),collapse = ', '),'.'))
      }
      method <- descriptions[[method]]
    }
  }
  return(method)
}

#' occupancy
#' @rdname occupancy
#' @description Return tibble containg proportional class occupancy for each feature for a given class info column.
#' @param x S4 object of class AnalysisData
#' @param cls info column to use for class data
#' @importFrom dplyr ungroup full_join
#' @export

setMethod('occupancy',signature = 'AnalysisData',
          function(x,cls = 'class'){
            
            feat <- tibble(Feature = features(x))
            
            d <- x %>%
            dat() %>%
           mutate(Class = clsExtract(x,cls))
            
            clsSize <- d %>%
              group_by(Class) %>%
              summarise(Frequency = n())
            
            d <- d %>%
              rowid_to_column(var = 'Sample') %>%
              gather('Feature','Intensity',-Class,-Sample) %>%
              filter(Intensity > 0)
            
            vars <- 'Class'
            names(vars) <- cls
            
            occ <- clsSize %>%
              base::split(seq_len(nrow(.))) %>%
              map(~{
                cla <- .
                cl <- d %>%
                  filter(Class == cla$Class) %>%
                  group_by(Class,Feature) %>%
                  summarise(N = n()) %>%
                  mutate(`Occupancy` = N / cla$Frequency)
              }) %>%
              bind_rows() %>%
              rename(!!vars) %>%
              ungroup()
            
            unoccupied <- feat %>%
              filter(!(Feature %in% occ$Feature)) %>%
              mutate(N = 0,Occupancy = 0,dummy = 1) %>%
              full_join(clsSize %>%
                          select(Class) %>%
                            rename(!!cls := Class) %>%
                            mutate(dummy = 1),by = 'dummy') %>%
              select(!!cls,Feature,N,Occupancy)
              
            occ <- occ %>%
              bind_rows(unoccupied) %>%
              arrange(!!sym(cls),Feature)
              
            return(occ)
          })
