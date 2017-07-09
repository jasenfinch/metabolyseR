library(FIEmspro)

context('fsMethods')

test_that('fsMethods returns methods correctly',{
  m <- sapply(metabolyseR:::fsMethods(),is.function)
  expect_false(F %in% m)
})

test_that('description names match method names',{
  d <- names(metabolyseR:::fsMethods(description = T))
  m <- names(metabolyseR:::fsMethods())
  expect_equal(d,m)
})

test_that('methods work',{
  m <- names(metabolyseR:::fsMethods())
  data("abr1")
  cls1 <- data.frame(cls = factor(abr1$fact$class[abr1$fact$class %in% c('1')]),abr1$neg[abr1$fact$class %in% c('1'),190:200])[1:10,]
  cls2 <- data.frame(cls = factor(abr1$fact$class[abr1$fact$class %in% c('6')]),abr1$neg[abr1$fact$class %in% c('6'),190:200])[1:10,]
  dat <- rbind(cls1,cls2)
  m <- lapply(m,function(x,dat){
    method <- metabolyseR:::fsMethods(x)
    res <- method(dat)
    return(res)
  },dat = dat)
  
  expect_false(F %in% sapply(m,function(x){class(x) == 'data.frame'}))
  expect_false(F %in% sapply(m,function(x){ncol(x) == 2}))
  expect_false(F %in% sapply(m,function(x){colnames(x) == c('Feature','Score')}))
  expect_false(F %in% sapply(m,function(x){class(x$Feature) == 'factor'}))
  expect_false(F %in% sapply(m,function(x){class(x$Score) == 'numeric'}))
})