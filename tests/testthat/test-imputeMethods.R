suppressPackageStartupMessages(library(FIEmspro))

context('imputeMethods')

test_that('imputeMethods returns methods correctly',{
  m <- sapply(metabolyseR:::imputeMethods(),is.function)
  expect_false(F %in% m)
})

test_that('imputeMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::imputeMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::imputeMethods(description = T))
  m <- names(metabolyseR:::imputeMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::imputeMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(metabolyseR:::imputeMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(metabolyseR:::imputeMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  skip_on_travis()
  m <- names(metabolyseR:::imputeMethods())
  data("abr1")
  dat <- list(Data = as_tibble(abr1$neg[abr1$fact$class %in% c('1','6'),500:600]), Info = as_tibble(cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),]))))
  m <- lapply(m,function(x,dat){
    method <- metabolyseR:::imputeMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(m,function(x){names(x) == c('Data','Info')}))
  expect_false(F %in% sapply(m,function(x){class(x[[1]]) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% sapply(m,function(x){class(x[[2]]) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% sapply(m,function(x,col){ncol(x$Data) == col},col = ncol(dat$Data)))
  expect_false(F %in% sapply(m,function(x,row){nrow(x$Data) == row},row = nrow(dat$Data)))
  expect_false(F %in% sapply(m,function(x,col){ncol(x$Info) == col},col = ncol(dat$Info)))
  expect_false(F %in% sapply(m,function(x,row){nrow(x$Info) == row},row = nrow(dat$Info)))
})
