library(metaboData)

data("abr1")

context('randomForest')

test_that('unsupervised random forest works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = NULL,nCores = 2)
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})

test_that('random forest classification works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = 'day',perm = 3,nCores = 2)
  
  plMDS <- plotMDS(rf$day,cls = 'day')
  plROC <- plotROC(rf$day)
  plMeasures <- plotMeasures(rf$day,predictor = 'day')
  plImportance <- plotImportance(rf$day)
  
  expect_true(is.list(rf))
  expect_true(class(rf$day) == 'RandomForest')
  expect_identical(class(plMDS),c("gg","ggplot"))
  expect_identical(class(plROC),c("gg","ggplot"))
  expect_identical(class(plMeasures),c("gg","ggplot"))
  expect_identical(class(plImportance),c("gg","ggplot"))
})

test_that('random forest regression works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  rf <- randomForest(d,cls = 'injorder',perm = 3,nCores = 2)
  
  expect_true(is.list(rf))
  expect_true(class(rf[[1]]) == 'RandomForest')
})