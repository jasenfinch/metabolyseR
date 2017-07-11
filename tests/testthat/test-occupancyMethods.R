suppressPackageStartupMessages(library(FIEmspro))

context('occupancyMethods')

test_that('occupancyMethods returns methods correctly',{
  m <- sapply(metabolyseR:::occupancyMethods(),is.function)
  expect_false(F %in% m)
})

test_that('occupancyMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::occupancyMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::occupancyMethods(description = T))
  m <- names(metabolyseR:::occupancyMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::occupancyMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(metabolyseR:::occupancyMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(metabolyseR:::occupancyMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(metabolyseR:::occupancyMethods())
  data("abr1")
  dat <- list(Data = abr1$neg, Info = abr1$fact)
  m <- lapply(m,function(x,dat){
    method <- metabolyseR:::occupancyMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(m,function(x){names(x) == c('Data','Info')}))
  expect_false(F %in% sapply(m,function(x){class(x[[1]]) == 'matrix'}))
  expect_false(F %in% sapply(m,function(x){class(x[[2]]) == 'data.frame'}))
  expect_false(F %in% sapply(m,function(x,row){nrow(x$Data) == row},row = nrow(dat$Data)))
  expect_false(F %in% sapply(m,function(x,col){ncol(x$Info) == col},col = ncol(dat$Info)))
  expect_false(F %in% sapply(m,function(x,row){nrow(x$Info) == row},row = nrow(dat$Info)))
})