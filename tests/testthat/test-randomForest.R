library(metaboData)

data("abr1")

context('randomForest')

test_that('unsupervised random forest works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = NULL)
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})

test_that('random forest classification works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = 'day',perm = 3)
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})

test_that('random forest regression works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = 'injorder')
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})