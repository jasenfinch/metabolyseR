
a <- new('Analysis')
raw(a) <- analysisData(metaboData::abr1$neg,
                       metaboData::abr1$fact)


context('sample information methods')

test_that('clsAvailable works',{
  cl <- clsAvailable(a,type = 'raw')
  expect_true(is.character(cl))
  expect_length(cl,9)
})

test_that('clsAvailable throws error when incorrect type specified',{
  expect_error(clsAvailable(a,type = 'wrong'))
})

test_that('clsExtract works',{
  cl <- clsExtract(
    a,
    cls = 'class',
    type = 'raw')
  expect_true(is.numeric(cl))
  expect_length(cl,120)
})

test_that('clsExtract errors when incorrect type specified',{
  expect_error(clsExtract(a,
                          cls = 'class',
                          type = 'wrong'))
})

test_that('clsReplace works',{
  b <- clsReplace(
    a,rep(1,120),
    cls = 'class',
    type = 'raw')
  i <- b %>%
    sinfo(type = 'raw')
  
  expect_equal(1,unique(i$class))
})

test_that('clsReplace errors when incorrect class column specified',{
  expect_error(clsReplace(a,
                          1:10,
                          cls = 'wrong'))
})

test_that('clsReplace errors when incorrect type specified',{
  expect_error(clsReplace(a,
                          1:10,
                          cls = 'class',
                          type = 'wrong'))
})

test_that('clsAdd works',{
  b <- clsAdd(
    a,
    'test',
    rep(1,120),
    type = 'raw')
  
  i <- b %>%
    sinfo(type = 'raw')
  
  expect_true('test' %in% clsAvailable(b,type = 'raw'))
  expect_equal(1,unique(i$test))
})

test_that('clsAdd throws error when class already present',{
  expect_error(clsAdd(a,'class',rep(1,120)))
})

test_that('clsAdd errors when incorrect type specified',{
  expect_error(clsAdd(a,
                      'class',
                      rep(1,120),
                      type = 'wrong'))
})

test_that('clsRemove works',{
  b <- clsRemove(a,'class',type = 'raw')
  expect_false('class' %in% clsAvailable(b))
})

test_that('clsRemove errors when incorrect class information column specified',{
  expect_error(clsRemove(a,cls = 'wrong'))
})

test_that('clsRemove errors when incorrect type specified',{
  expect_error(clsRemove(a,type = 'wrong'))
})

test_that('clsArrange works',{
  b <- clsArrange(a,'class',type = 'raw')
  expect_identical(clsExtract(a,type = 'raw') %>%
                     sort(),
                   clsExtract(b,type = 'raw'))
})

test_that('clsArrange works for decending arrangment',{
  b <- clsArrange(a,'class',
                  descending = TRUE,
                  type = 'raw')
  expect_identical(clsExtract(a,type = 'raw') %>%
                     sort(decreasing = TRUE),
                   clsExtract(b,type = 'raw'))
})

test_that('clsArrange errors when incorrect type specified',{
  expect_error(clsArrange(a,type = 'wrong'))
})

test_that('clsRename works',{
  b <- clsRename(a,'rep','replicate',type = 'raw')
  expect_true('replicate' %in% clsAvailable(b,type = 'raw'))
})

test_that('clsRename errors when incorrect type specified',{
  expect_error(clsRename(a,type = 'wrong'))
})
