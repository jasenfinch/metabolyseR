
context('changeParameter')

test_that('changeParameter changes all parameters present',{
  p <- analysisParameters()
  p <- changeParameter('nCores',2,p)
  
  expect_true(p@preTreat$impute$class$nCores == 2)
  expect_true(p@classification$nCores == 2)
  expect_true(p@featureSelection$nCores == 2)
})