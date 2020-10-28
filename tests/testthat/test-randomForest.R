library(metaboData)

d <- analysisData(abr1$neg,abr1$fact) %>%
  keepFeatures(features = features(.)[200:250])

context('randomForest')

test_that('unsupervised random forest works',{
  rf <- randomForest(d,cls = NULL,nCores = 2)
  
  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'unsupervised')
})

test_that('random forest classification works',{
  rf <- randomForest(d,cls = 'day',perm = 3,nCores = 2,returnModels = TRUE)

  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'classification')
})

test_that('binary classification works',{
  d <- d %>%
    keepClasses(cls = 'day',classes = c('H','5'))
  
  rf <- randomForest(d,cls = 'day',nCores = 2,binary = TRUE)
  
  expect_s4_class(rf,'RandomForest')
})

test_that('random forest regression works',{
  rf <- randomForest(d,cls = 'injorder',perm = 3,nCores = 2)
  
  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'regression')
})
