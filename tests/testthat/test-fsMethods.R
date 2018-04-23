suppressPackageStartupMessages(library(FIEmspro))

context('fsMethods')

test_that('fsMethods returns methods correctly',{
  m <- sapply(metabolyseR:::fsMethods(),is.function)
  expect_false(F %in% m)
})

test_that('fsMethods returns descriptions correctly',{
  m <- sapply(metabolyseR:::fsMethods(description = T),is.list)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::fsMethods(description = T))
  m <- names(metabolyseR:::fsMethods())
  expect_equal(d,m)
})

test_that('descriptions have correct names', {
  n <- lapply(metabolyseR:::fsMethods(description = T),names)
  expect_false(F %in% unlist(lapply(n,function(x){x == c('description','arguments','score','Pvalue')})))
})

test_that('number of method arguments matches description arguments', {
  d <- sapply(metabolyseR:::fsMethods(description = T),function(x){length(x$arguments)})
  m <- sapply(metabolyseR:::fsMethods(),function(x){length(formals(x)[-1])})
  expect_equal(d,m)
})

test_that('methods work',{
  meth <- names(metabolyseR:::fsMethods())
  data("abr1")
  cls1 <- data.frame(cls = factor(abr1$fact$class[abr1$fact$class %in% c('1')]),abr1$neg[abr1$fact$class %in% c('1'),190:200])[1:10,]
  cls2 <- data.frame(cls = factor(abr1$fact$class[abr1$fact$class %in% c('6')]),abr1$neg[abr1$fact$class %in% c('6'),190:200])[1:10,]
  dat <- rbind(cls1,cls2)
  m <- lapply(meth,function(x,dat){
    method <- metabolyseR:::fsMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  names(m) <- meth
  
  expect_false(F %in% sapply(m,function(x){class(x) == c('tbl_df','tbl','data.frame')}))
  expect_false(F %in% sapply(m,function(x){ncol(x) == 3}))
  expect_false(F %in% sapply(m,function(x){colnames(x) == c('Feature','Score','Pvalue')}))
  expect_false(F %in% sapply(m,function(x){class(x$Feature) == 'character'}))
  expect_false(F %in% sapply(m,function(x){class(x$Score) == 'numeric'}))
  expect_false(F %in% sapply(m,function(x){class(x$Pvalue) == 'numeric'}))
  expect_false(F %in% sapply(m,function(x,nFeat){nrow(x) == nFeat},nFeat = ncol(dat) - 1))
})
