library(metaboData)

context('imputeMethods')

test_that('imputeMethods returns methods correctly',{
  m <- sapply(imputeMethods(),is.function)
  expect_false(F %in% m)
})

test_that('imputeMethods returns descriptions correctly',{
  m <- sapply(imputeMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(imputeMethods(description = T))
  m <- names(imputeMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(imputeMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(imputeMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(imputeMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(imputeMethods())
  d <- analysisData(abr1$neg[abr1$fact$class %in% c('1','6'),500:600],cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),])))
  m <- lapply(m,function(x,dat){
    method <- imputeMethods(x)
    res <- method(d,nCores = 1)
    return(res)
  },d = d)
  
  expect_false(F %in% sapply(m,function(x){names(x) == c('Data','Info')}))
  expect_false(F %in% sapply(m,function(x){class(dat(x)) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% sapply(m,function(x){class(info(x)) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% sapply(m,function(x,col){ncol(dat(x)) == col},col = ncol(dat(d))))
  expect_false(F %in% sapply(m,function(x,row){nrow(dat(x)) == row},row = nrow(dat(d))))
  expect_false(F %in% sapply(m,function(x,col){ncol(info(x)) == col},col = ncol(info(d))))
  expect_false(F %in% sapply(m,function(x,row){nrow(info(x)) == row},row = nrow(info(d))))
})
