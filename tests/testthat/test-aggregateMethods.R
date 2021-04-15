library(metaboData)

context('aggregateMethods')

test_that('aggregateMethods returns methods correctly',{
  m <- map_lgl(aggregateMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('methods work',{
  m <- names(aggregateMethods())
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[500:600]) %>%
    keepClasses(classes = c(1,6))
  
  m <- lapply(m,function(x,dat){
    method <- aggregateMethods(x)
    res <- method(dat)
    return(res)
  },dat = d)
  expect_false(FALSE %in% map_lgl(
    m,~{identical(slotNames(.x),c('data','info'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{class(.x) == 'AnalysisData'}))
  expect_false(FALSE %in% (map_dbl(
    m,~{nrow(.x %>% 
               dat())}) == map_dbl(
                 m,
                 ~{nrow(.x %>% 
                          sinfo())})))
})

