library(metaboData)

data("abr1")

d <- analysisData(abr1$neg[,200:250],abr1$fact)

context('randomForest')

test_that('unsupervised random forest works',{
  rf <- randomForest(d,cls = NULL,nCores = 2)
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})

test_that('random forest classification works',{
  rf <- randomForest(d,cls = 'day',perm = 3,nCores = 2,returnModels = TRUE)

  expect_true(is.list(rf))
  expect_true(class(rf$day) == 'RandomForest')
})

test_that('binary classification works',{
  d <- d %>%
    keepClasses(cls = 'day',classes = c('H','5'))
  
  rf <- randomForest(d,cls = 'day',nCores = 2,binary = TRUE)
  
  expect_true(is.list(rf))
  expect_true(class(rf$day) == 'RandomForest')
})

test_that('random forest regression works',{
  rf <- randomForest(d,cls = 'injorder',perm = 3,nCores = 2)
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})