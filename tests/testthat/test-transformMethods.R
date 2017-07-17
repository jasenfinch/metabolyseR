suppressPackageStartupMessages(library(FIEmspro))

context('transformMethods')

test_that('transformMethods returns methods correctly',{
  m <- sapply(metabolyseR:::transformMethods(),is.function)
  expect_false(F %in% m)
})

test_that('transformMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::transformMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::transformMethods(description = T))
  m <- names(metabolyseR:::transformMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::transformMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('methods work',{
  m <- names(metabolyseR:::transformMethods())
  data("abr1")
  dat <- list(Data = abr1$neg[abr1$fact$class %in% c('1','6'),500:600], Info = cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),])))
  m <- lapply(m,function(x,dat){
    method <- metabolyseR:::transformMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(m,function(x){names(x) == c('Data','Info')}))
  expect_false(F %in% sapply(m,function(x){class(x[[1]]) == 'matrix'}))
  expect_false(F %in% sapply(m,function(x){class(x[[2]]) == 'data.frame'}))
  expect_false(F %in% sapply(m,function(x,col){ncol(x$Data) == col},col = ncol(dat$Data)))
  expect_false(F %in% sapply(m,function(x,row){nrow(x$Data) == row},row = nrow(dat$Data)))
  expect_false(F %in% sapply(m,function(x,col){ncol(x$Info) == col},col = ncol(dat$Info)))
  expect_false(F %in% sapply(m,function(x,row){nrow(x$Info) == row},row = nrow(dat$Info)))
})