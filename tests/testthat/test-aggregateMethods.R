library(metaboData)

context('aggregateMethods')

test_that('aggregateMethods returns methods correctly',{
  m <- map_lgl(aggregateMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('aggregateMethods returns descriptions correctly',{
  m <- map_lgl(aggregateMethods(description = TRUE),is.list)
  expect_false(FALSE %in% m)
})

test_that('description names match method names',{
  d <- names(aggregateMethods(description = TRUE))
  m <- names(aggregateMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(aggregateMethods(description = TRUE),names)
  expect_false(FALSE %in% unlist(lapply(
    n,
    function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- map_dbl(aggregateMethods(description = TRUE),
               ~{length(.x$arguments)})
  m <- map_dbl(aggregateMethods(),~{length(formals(.x)[-1])})
  expect_equal(d,m)
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
  expect_identical(FALSE %in% map_lgl(
    m,~{slotNames(.x) == c('data','info')}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{class(.x) == 'AnalysisData'}))
  expect_false(FALSE %in% (map_dbl(
    m,~{nrow(x %>% 
               dat())}) == map_dbl(
                 m,
                 ~{nrow(.x %>% 
                          sinfo())})))
})

