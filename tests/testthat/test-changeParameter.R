
context('changeParameter')

test_that('changeParameter changes all parameters present',{
  p <- analysisParameters()
  p <- changeParameter(p,'nCores',2)
  
  expect_true(p@preTreat$impute$class$nCores == 2)
  expect_true(p@modelling$randomForest$nCores == 2)
})