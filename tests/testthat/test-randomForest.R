library(metaboData)

d <- analysisData(abr1$neg,abr1$fact) %>%
  keepFeatures(features = features(.)[200:250])

context('randomForest')

test_that('unsupervised random forest works',{
  rf <- randomForest(d,cls = NULL,returnModels = TRUE)
  
  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'unsupervised')
})

test_that('random forest classification works',{
  expect_warning(rf <- randomForest(d,cls = c('day','name'),perm = 3,returnModels = TRUE))
  
  rf_metrics <- metrics(rf)
  rf_importance <- importance(rf)
  
  rf_wrong <- c(rf,list('wrong'))
  
  expect_identical(class(rf),'list')
  expect_identical(type(rf$day),'classification')
  expect_s3_class(rf_metrics,'tbl_df')
  expect_s3_class(rf_importance,'tbl_df')
  expect_error(metrics(rf_wrong))
  expect_error(importance(rf_wrong))
})

test_that('binary classification works',{
  d <- d %>%
    keepClasses(cls = 'day',classes = c('H','5'))
  
  rf <- randomForest(d,cls = 'day',binary = TRUE)
  
  expect_s4_class(rf,'RandomForest')
})

test_that('random forest regression works',{
  rf <- randomForest(d,cls = 'injorder',perm = 3,returnModels = TRUE)
  
  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'regression')
})

test_that('low sample permutation testing works',{
  expect_warning(rf <- analysisData(data = abr1$neg,info = abr1$fact) %>%
    keepSamples(idx = 'injorder',samples = 1:2) %>%
    randomForest(cls = 'injorder',perm = 1000))
  
  expect_s4_class(rf,'RandomForest')
})

test_that('specifiying comparisons works',{
  rf <- randomForest(d,comparisons = list(day = 'H~5'),
                     cls = 'day')
  
  expect_s4_class(rf,'RandomForest')
})
