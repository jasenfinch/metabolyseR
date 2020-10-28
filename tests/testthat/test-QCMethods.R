library(metaboData)

context('QCMethods')

test_that('QCMethods returns methods correctly',{
  m <- map_lgl(QCMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('QCMethods returns descriptions correctly',{
  m <- map_lgl(QCMethods(description = TRUE),is.list)
  expect_false(FALSE %in% m)
})

test_that('description names match method names',{
  d <- names(QCMethods(description = TRUE))
  m <- names(QCMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(QCMethods(description = TRUE),names)
  expect_false(FALSE %in% unlist(lapply(
    n,
    function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- map_dbl(QCMethods(description = TRUE),
              ~{length(.x$arguments)})
  m <- map_dbl(QCMethods(),~{length(formals(.x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(QCMethods())
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)) %>%
    keepClasses(classes = c(1,6))
  
  m <- map(m,~{
    method <- QCMethods(.)
    if (. == 'impute') {
      res <- method(d, cls = 'class', QCidx = '1',nCores = 2)
    } else {
      res <- method(d, cls = 'class', QCidx = '1')  
    }
    
    return(res)
  })
  
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(slotNames(.x),c('data','info'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(dat(.x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(sinfo(.x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{ncol(sinfo(x)) == ncol(sinfo(d))}))
})
