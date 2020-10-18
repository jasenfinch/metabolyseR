
context('plotUnsupervisedRF')

test_that('plotUnsupervisedRF works',{
  
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250])
  
  pl <- plotUnsupervisedRF(d,cls = 'day')
  
  expect_true(identical(class(pl),c("gg","ggplot")))
})
