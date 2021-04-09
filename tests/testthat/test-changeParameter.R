
context('changeParameter')

test_that('changeParameter changes all parameters present',{
  p <- analysisParameters()
  changeParameter(p,'nCores') <- 2
  
  expect_true(parameters(p,'pre-treatment')$impute$class$nCores == 2)
  expect_true(parameters(p,'modelling')$randomForest$nCores == 2)
})
