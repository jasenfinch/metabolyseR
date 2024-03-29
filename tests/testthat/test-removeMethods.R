library(metaboData)

context('removeMethods')

test_that('removeMethods returns methods correctly',{
  m <- map_lgl(removeMethods(),is.function)
  expect_false(F %in% m)
})

test_that('removeMethods errors if incorrect method specified',{
  expect_error(removeMethods('incorrect'))
})

test_that('methods work',{

  d <- analysisData(abr1$neg,abr1$fact)
  method <- removeMethods('samples')
  s <- method(d,idx = 'injorder', samples = c(1,2))
  method <- removeMethods('classes')
  cl <- method(d, cls = 'class', classes = c('1'))
  method <- removeMethods('features')
  var <- method(d, features = 'N133')
  
  expect_equal(slotNames(s), c('data','info'))
  expect_equal(slotNames(cl), c('data','info'))
  expect_equal(slotNames(var), c('data','info'))
  
  expect_true(identical(class(dat(s)), c('tbl_df','tbl','data.frame')))
  expect_true(identical(class(dat(cl)), c('tbl_df','tbl','data.frame')))
  expect_true(identical(class(dat(var)), c('tbl_df','tbl','data.frame')))
  
  expect_true(identical(class(sinfo(s)), c('tbl_df','tbl','data.frame')))
  expect_true(identical(class(sinfo(cl)), c('tbl_df','tbl','data.frame')))
  expect_true(identical(class(sinfo(var)), c('tbl_df','tbl','data.frame')))
  
  expect_equal(ncol(dat(s)),ncol(dat(d)))
  expect_equal(ncol(sinfo(s)),ncol(sinfo(d)))
  
  expect_equal(ncol(dat(cl)),ncol(dat(d)))
  expect_equal(ncol(sinfo(cl)),ncol(sinfo(d)))
  
  expect_equal(ncol(dat(var)),ncol(dat(d)) - 1)
  expect_equal(ncol(sinfo(var)),ncol(sinfo(d)))
  
  expect_equal(nrow(dat(s)),nrow(dat(d)) - 2)
  expect_equal(nrow(sinfo(s)),nrow(sinfo(d)) - 2)
  
  expect_equal(nrow(dat(cl)),nrow(dat(d)) - 20)
  expect_equal(nrow(sinfo(cl)),nrow(sinfo(d)) - 20)
  
  expect_equal(nrow(dat(var)),nrow(dat(d)))
  expect_equal(nrow(sinfo(var)),nrow(sinfo(d)))
})
