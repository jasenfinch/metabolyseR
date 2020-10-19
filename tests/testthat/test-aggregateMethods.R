library(metaboData)

context('aggregateMethods')

test_that('aggregateMethods returns methods correctly',{
  m <- sapply(aggregateMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('aggregateMethods returns descriptions correctly',{
  m <- sapply(aggregateMethods(description = TRUE),is.list)
  expect_false(FALSE %in% m)
})

test_that('description names match method names',{
  d <- names(aggregateMethods(description = TRUE))
  m <- names(aggregateMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(aggregateMethods(description = TRUE),names)
  expect_false(FALSE %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(aggregateMethods(description = TRUE),function(x){length(x$arguments)})
  m <- sapply(aggregateMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(aggregateMethods())
  dat <- analysisData(abr1$neg[abr1$fact$class %in% c('1','6'),500:600], cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = seq_len(nrow(abr1$fact[abr1$fact$class %in% c('1','6'),]))))
  m <- lapply(m,function(x,dat){
    method <- aggregateMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(FALSE %in% sapply(m,function(x){slotNames(x) == c('data','info')}))
  expect_false(FALSE %in% sapply(m,function(x){class(x) == 'AnalysisData'}))
  expect_false(FALSE %in% (sapply(m,function(x){nrow(x %>% dat())}) == sapply(m,function(x){nrow(x %>% sinfo())})))
})

