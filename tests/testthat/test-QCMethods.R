library(metaboData)

context('QCMethods')

test_that('QCMethods returns methods correctly',{
  m <- sapply(QCMethods(),is.function)
  expect_false(F %in% m)
})

test_that('QCMethods returns descriptions correctly',{
  m <- sapply(QCMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(QCMethods(description = T))
  m <- names(QCMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(QCMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(QCMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(QCMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(QCMethods())
  data("abr1")
  d <- analysisData(abr1$neg[abr1$fact$class %in% c('1','6'),500:600], 
              cbind(abr1$fact[abr1$fact$class %in% c('1','6'),],
                                     fileOrder = 1:nrow(abr1$fact[abr1$fact$class %in% c('1','6'),])))
  m <- map(m,~{
    method <- QCMethods(.)
    if (. == 'impute') {
      res <- method(d, cls = 'class', QCidx = '1',nCores = 2)
    } else {
      res <- method(d, cls = 'class', QCidx = '1')  
    }
    
    return(res)
  })
  
  expect_false(F %in% sapply(m,function(x){identical(slotNames(x),c('data','info'))}))
  expect_false(F %in% sapply(m,function(x){identical(class(dat(x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(F %in% sapply(m,function(x){identical(class(sinfo(x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(F %in% sapply(m,function(x,col){ncol(sinfo(x)) == col},col = ncol(sinfo(d))))
})
