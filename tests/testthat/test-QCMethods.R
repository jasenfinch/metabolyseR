
test_that('QCMethods returns methods correctly',{
  m <- map_lgl(QCMethods(),is.function)
  expect_false(FALSE %in% m)
})

test_that('QCMethods errors if incorrect method specified',{
  expect_error(QCMethods('incorrect'))
})

test_that('methods work',{
  m <- names(QCMethods())
  d <- analysisData(
    metaboData::abr1$neg,
    metaboData::abr1$fact) %>%
    keepFeatures(features = features(.)[290:300]) %>%
    keepClasses(classes = c(1,6))
  
  m <- map(m,~{
    method <- QCMethods(.)
    if (.x == 'impute'){
      method(d, cls = 'class', QCidx = '1',parallel = 'no')
    } else {
      method(d, cls = 'class', QCidx = '1') 
    }
  })
  
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(slotNames(.x),c('data','info'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(dat(.x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{identical(class(sinfo(.x)),c('tbl_df','tbl','data.frame'))}))
  expect_false(FALSE %in% map_lgl(
    m,
    ~{ncol(sinfo(.x)) == ncol(sinfo(d))}))
})
