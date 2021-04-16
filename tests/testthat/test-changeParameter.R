
context('changeParameter')

test_that('changeParameter changes all parameters present',{
  p <- analysisParameters()
  changeParameter(p,'cls') <- 'day'
  
  expect_equal(parameters(p,'pre-treatment')$QC$occupancyFilter$cls,'day')
  expect_equal(parameters(p,'modelling')$randomForest$cls,'day')
})
