library(metaboData)

a <- new('Analysis')
raw(a) <- analysisData(abr1$neg,abr1$fact)


context('sample information methods')

test_that('clsAvailable works',{
 cl <- clsAvailable(a)
 expect_true(is.character(cl))
 expect_length(cl,9)
})

test_that('clsAvailable throws error when incorrect type specified',{
  expect_error(clsAvailable(a,type = 'wrong'))
})

test_that('clsExtract works',{
  cl <- clsExtract(a,cls = 'class')
  expect_true(is.numeric(cl))
  expect_length(cl,120)
})

test_that('clsReplace works',{
  b <- clsReplace(a,rep(1,120),cls = 'class')
  i <- b %>%
    sinfo(type = 'raw')
  
  expect_equal(1,unique(i$class))
})

test_that('clsAdd works',{
  b <- clsAdd(a,'test',rep(1,120))
  i <- b %>%
    sinfo(type = 'raw')
  expect_true('test' %in% clsAvailable(b))
  expect_equal(1,unique(i$test))
})

test_that('clsAdd throws error when class already present',{
  expect_error(clsAdd(a,'class',rep(1,120)))
})

test_that('clsRemove works',{
  b <- clsRemove(a,'class')
  expect_false('class' %in% clsAvailable(b))
})

test_that('clsArrange works',{
  b <- clsArrange(a,'class')
  expect_identical(clsExtract(a) %>%
                     sort(),
                   clsExtract(b))
})

test_that('clsRename works',{
  b <- clsRename(a,'rep','replicate')
  expect_true('replicate' %in% clsAvailable(b))
})
