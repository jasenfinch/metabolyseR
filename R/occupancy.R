#' Feature occupancy filtering
#' @rdname occupancyFilter
#' @description Feature filtering based on class occupancy.
#' @param d S4 object of class `AnalysisData`
#' @param cls sample information column name to use for class data
#' @param occupancy feature occupancy filtering threshold, below which features will be removed
#' @return An S4 object of class `AnalysisData` containing the class occupancy filtered data.
#' @details 
#' Occupancy provides a useful metric by which to filter poorly represented features (features containing a majority zero or missing values).
#' An occupancy threshold provides a means of specifying this majority with variables below the threshold excluded from further analyses.
#' However, this can be complicated by an underlying class structure present within the data where a variable may be well represented within one class but not in another.
#' @section Methods:
#' * `occupancyMaximium`: Maximum occupancy threshold feature filtering. Where the maximum occupancy across all classes is above the threshold. Therefore, for a feature to be retained, only a single class needs to have an occupancy above the threshold.
#' * `occupancyMinimum`: Minimum occupancy threshold feature filtering. Where the minimum occupancy across all classes is required to be above the threshold. Therefore, for a feature to be retained, all classes would need to have an occupancy above the threshold.
#' @examples 
#' ## Each of the following examples shows the application of the feature occupancy filtering method method and then 
#' ## a Principle Component Analysis is plotted to show it's effect on the data structure.
#' 
#' ## Initial example data preparation
#' library(metaboData)
#' 
#' d <- analysisData(abr1$neg[,200:300],abr1$fact)
#'  
#' ## Maximum occupancy threshold feature filtering
#' d %>% 
#'  occupancyMaximum(cls = 'day') %>% 
#'  plotPCA(cls = 'day')
#'  
#' ## Minimum occupancy threshold feature filtering
#' d %>% 
#'  occupancyMinimum(cls = 'day') %>% 
#'  plotPCA(cls = 'day')
#' @export

setGeneric("occupancyMaximum", function(d, cls = 'class', occupancy = 2/3) {
  standardGeneric("occupancyMaximum")
})

#' @rdname occupancyFilter

setMethod('occupancyMaximum',signature = 'AnalysisData',
          function(d,cls = 'class', occupancy = 2/3){
            occ <- occupancy(d,cls)
            fd <- occ %>%
              group_by(Feature) %>%
              summarise(Occupancy = max(Occupancy)) %>%
              filter(Occupancy >= occupancy)
            feat <- colnames(d %>% 
                               dat())[colnames(d %>% 
                                               dat()) %in% 
                                      unique(fd$Feature)]
            dat(d) <- d %>%
              dat() %>%
              select(feat)
            return(d)
          }
)

#' @rdname occupancyFilter
#' @export

setGeneric("occupancyMinimum", function(d, cls = 'class', occupancy = 2/3) {
  standardGeneric("occupancyMinimum")
})

#' @rdname occupancyFilter

setMethod('occupancyMinimum',signature = 'AnalysisData',
          function(d,cls = 'class', occupancy = 2/3){
            occ <- occupancy(d,cls)
            fd <- occ %>%
              group_by(Feature) %>%
              summarise(Occupancy = min(Occupancy)) %>%
              filter(Occupancy >= occupancy)
            feat <- colnames(d %>% 
                               dat())[colnames(d %>% 
                                                 dat()) %in% 
                                        unique(fd$Feature)]
            dat(d) <- d %>% 
              dat() %>%
              select(feat)
            return(d)
          }
)

#' occupancy
#' @rdname occupancy
#' @description Return tibble containg proportional class occupancy 
#' for each feature for a given class info column.
#' @param d S4 object of class AnalysisData
#' @param cls info column to use for class data
#' @importFrom dplyr ungroup full_join
#' @export

setGeneric("occupancy", function(d, cls = 'class') {
  standardGeneric("occupancy")
})

#' @rdname occupancy

setMethod('occupancy',signature = 'AnalysisData',
          function(d,cls = 'class'){
            
            feat <- tibble(Feature = features(d))
            
            d <- d %>%
              dat() %>%
              mutate(Class = clsExtract(d,cls))
            
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
