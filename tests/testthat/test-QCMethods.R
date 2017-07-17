suppressPackageStartupMessages(library(FIEmspro))

context('QCMethods')

test_that('QCMethods returns methods correctly',{
  m <- sapply(metabolyseR:::QCMethods(),is.function)
  expect_false(F %in% m)
})

test_that('QCMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::QCMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::QCMethods(description = T))
  m <- names(metabolyseR:::QCMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::QCMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(metabolyseR:::QCMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(metabolyseR:::QCMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(metabolyseR:::QCMethods())
  data("abr1")
  dat <- list(Data = abr1$neg[abr1$fact$class %in% c('1','6'),500:600], Info = cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),])))
  m <- lapply(m,function(x,dat){
    method <- metabolyseR:::QCMethods(x)
    res <- method(dat, cls = 'class', idx = '1')
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(m,function(x){names(x) == c('Data','Info')}))
  expect_false(F %in% sapply(m,function(x){class(x[[1]]) == 'matrix'}))
  expect_false(F %in% sapply(m,function(x){class(x[[2]]) == 'data.frame'}))
  expect_false(F %in% sapply(m,function(x,col){ncol(x$Info) == col},col = ncol(dat$Info)))
})
