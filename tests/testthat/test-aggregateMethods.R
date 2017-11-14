suppressPackageStartupMessages(library(FIEmspro))

context('aggregateMethods')

test_that('aggregateMethods returns methods correctly',{
  m <- sapply(metabolyseR:::aggregateMethods(),is.function)
  expect_false(F %in% m)
})

test_that('aggregateMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::aggregateMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::aggregateMethods(description = T))
  m <- names(metabolyseR:::aggregateMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::aggregateMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(metabolyseR:::aggregateMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(metabolyseR:::aggregateMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(metabolyseR:::aggregateMethods())
  data("abr1")
  dat <- list(Data = as_tibble(abr1$neg[abr1$fact$class %in% c('1','6'),500:600]), Info = as_tibble(cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),]))))
  m <- lapply(m,function(x,dat){
    method <- metabolyseR:::aggregateMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(m,function(x){names(x) == c('Data','Info')}))
  expect_false(F %in% sapply(m,function(x){class(x[[1]]) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% sapply(m,function(x){class(x[[2]]) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% (sapply(m,function(x){nrow(x$Data)}) == sapply(m,function(x){nrow(x$Info)})))
})
