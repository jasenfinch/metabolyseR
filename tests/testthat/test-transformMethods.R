library(metaboData)

context('transformMethods')

test_that('transformMethods returns methods correctly',{
  m <- sapply(transformMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('transformMethods returns descriptions correctly',{
  m <- sapply(transformMethods(description = TRUE),is.list)
  expect_false(FALSE %in% m)
})

test_that('description names match method names',{
  d <- names(transformMethods(description = TRUE))
  m <- names(transformMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(transformMethods(description = TRUE),names)
  expect_false(FALSE %in% unlist(lapply(
    n,
    function(x){x == c('description','arguments')})))
})

test_that('methods work',{
  m <- names(transformMethods())
  
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[500:600]) %>%
    keepClasses(classes = c('1','6'))
  
  m <- map(m,~{
    method <- transformMethods(.)
    res <- method(d)
    return(res)
  })
  
  expect_false(FALSE %in% sapply(
    m,
    function(x){identical(slotNames(x),c('data','info'))}))
  expect_false(FALSE %in% sapply(
    m,
    function(x){identical(class(x %>% dat()),c('tbl_df',"tbl","data.frame"))}))
  expect_false(FALSE %in% sapply(
    m,
    function(x){identical(class(x %>% sinfo()),
                          c('tbl_df',"tbl","data.frame"))}))
  expect_false(FALSE %in% sapply(
    m,
    function(x,col){ncol(dat(x)) == col},col = ncol(dat(d))))
  expect_false(FALSE %in% sapply(
    m,
    function(x,row){nrow(dat(x)) == row},row = nrow(dat(d))))
  expect_false(FALSE %in% sapply(
    m,
    function(x,col){ncol(sinfo(x)) == col},col = ncol(sinfo(d))))
  expect_false(FALSE %in% sapply(
    m,
    function(x,row){nrow(sinfo(x)) == row},row = nrow(dat(d))))
})