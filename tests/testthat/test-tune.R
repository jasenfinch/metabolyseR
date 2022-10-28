
x <- analysisData(metaboData::abr1$neg[,200:300],
                  metaboData::abr1$fact) %>%
  occupancyMaximum(cls = 'day') %>%
  transformTICnorm()

test_that("tuning works", {
  tune_values <- tune(x,cls = 'day')
  
  expect_equal(tune_values$mtry,9)
  expect_equal(tune_values$ntree,1000)
})

test_that('tuning throws an error for unsupervised random forest',{
  expect_error(tune(x,cls = NULL))
})

test_that('an empty list is returned when no optimal parameters can be found',{
  tune_values <- tune(x,
                      cls = 'day',
                      ntree_range = 1)
  expect_identical(tune_values,list())
})
