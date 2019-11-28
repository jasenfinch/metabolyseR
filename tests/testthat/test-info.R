library(metaboData)

a <- new('Analysis')
raw(a) <- analysisData(abr1$neg,abr1$fact)


context('sample information methods')

test_that('availableCls works',{
 cl <- availableCls(a)
 expect_true(is.character(cl))
 expect_length(cl,9)
})

test_that('extractCls works',{
  cl <- extractCls(a,cls = 'class')
  expect_true(is.numeric(cl))
  expect_length(cl,120)
})

test_that('replaceCls works',{
  b <- replaceCls(a,rep(1,120),cls = 'class')
  i <- b %>%
    rawInfo()
  
  expect_equal(1,unique(i$class))
})

test_that('addCls works',{
  b <- addCls(a,'test',rep(1,120))
  i <- b %>%
    rawInfo()
  expect_true('test' %in% availableCls(b))
  expect_equal(1,unique(i$test))
})

test_that('removeCls works',{
  b <- removeCls(a,'class')
  expect_false('class' %in% availableCls(b))
})