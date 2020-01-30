library(metaboData)

a <- new('Analysis')
raw(a) <- analysisData(abr1$neg,abr1$fact)


context('sample information methods')

test_that('clsAvailable works',{
 cl <- clsAvailable(a)
 expect_true(is.character(cl))
 expect_length(cl,9)
})

test_that('clsExtract works',{
  cl <- clsExtract(a,cls = 'class')
  expect_true(is.numeric(cl))
  expect_length(cl,120)
})

test_that('clsReplace works',{
  b <- clsReplace(a,rep(1,120),cls = 'class')
  i <- b %>%
    rawInfo()
  
  expect_equal(1,unique(i$class))
})

test_that('clsAdd works',{
  b <- clsAdd(a,'test',rep(1,120))
  i <- b %>%
    rawInfo()
  expect_true('test' %in% clsAvailable(b))
  expect_equal(1,unique(i$test))
})

test_that('clsRemove works',{
  b <- clsRemove(a,'class')
  expect_false('class' %in% clsAvailable(b))
})