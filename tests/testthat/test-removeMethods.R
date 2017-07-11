suppressPackageStartupMessages(library(FIEmspro))

context('removeMethods')

test_that('removeMethods returns methods correctly',{
  m <- sapply(metabolyseR:::removeMethods(),is.function)
  expect_false(F %in% m)
})

test_that('removeMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::removeMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::removeMethods(description = T))
  m <- names(metabolyseR:::removeMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::removeMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(metabolyseR:::removeMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(metabolyseR:::removeMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  data("abr1")
  dat <- list(Data = abr1$neg, Info = abr1$fact)
  method <- metabolyseR:::removeMethods('sample')
  s <- method(dat,idx = 'injorder', samples = c(1,2))
  method <- metabolyseR:::removeMethods('class')
  cl <- method(dat, cls = 'class', classes = c('1'))
  
  expect_equal(names(s), c('Data','Info'))
  expect_equal(names(cl), c('Data','Info'))
  
  expect_equal(class(s$Data), 'matrix')
  expect_equal(class(cl$Data), 'matrix')
  
  expect_equal(class(s$Info), 'data.frame')
  expect_equal(class(cl$Info), 'data.frame')
  
  expect_equal(ncol(s$Data),ncol(dat$Data))
  expect_equal(ncol(s$Info),ncol(dat$Info))
  
  expect_equal(ncol(cl$Data),ncol(dat$Data))
  expect_equal(ncol(cl$Info),ncol(dat$Info))
  
  expect_equal(nrow(s$Data),nrow(dat$Data) - 2)
  expect_equal(nrow(s$Info),nrow(dat$Info) - 2)
  
  expect_equal(nrow(cl$Data),nrow(dat$Data) - 20)
  expect_equal(nrow(cl$Info),nrow(dat$Info) - 20)
})
