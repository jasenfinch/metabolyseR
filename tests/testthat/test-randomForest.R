
d <- analysisData(metaboData::abr1$neg,
                  metaboData::abr1$fact) %>%
  keepFeatures(features = features(.)[200:250]) %>% 
  clsAdd('sample_names',paste0(seq_len(nSamples(.)),'_a'))

test_that('unsupervised random forest works',{
  rf <- randomForest(d,cls = NULL,returnModels = TRUE)
  
  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'unsupervised')
  expect_equal(mtry(rf,cls = NULL),7)
})

test_that('random forest classification works',{
  expect_warning(rf <- randomForest(d,cls = c('day','name'),perm = 2,returnModels = TRUE))
  rf_no_perm <- randomForest(d,cls = 'day')
  
  rf_metrics <- metrics(rf)
  rf_predictions <- predictions(rf)
  rf_importance <- importance(rf)
  rf_proximity <- proximity(rf,idx = 'sample_names')
  
  rf_wrong <- c(rf,list('wrong'))
  
  expect_identical(class(rf),'list')
  expect_identical(type(rf$day),'classification')
  expect_s3_class(rf_metrics,'tbl_df')
  expect_s3_class(rf_predictions,'tbl_df')
  expect_s3_class(rf_importance,'tbl_df')
  expect_s3_class(rf_proximity,'tbl_df')
  expect_s3_class(explanatoryFeatures(rf,metric = 'MeanDecreaseAccuracy'),'tbl_df')
  
  expect_s3_class(metrics(rf_wrong),'tbl_df')
  expect_s3_class(predictions(rf_wrong),'tbl_df')
  expect_error(importance(rf_wrong))
  expect_s3_class(proximity(rf_wrong),'tbl_df')
  expect_error(explanatoryFeatures(rf_wrong))
  
  expect_equal(nrow(metrics(list(0,0))),0)
  expect_equal(nrow(predictions(list(0,0))),0)
  expect_equal(nrow(proximity(list(0,0))),0)
  
  expect_error(proximity(rf,idx = 'name'))
  expect_error(explanatoryFeatures(rf,metric = 'wrong'))
  expect_error(explanatoryFeatures(rf_no_perm,value = 'p-value'))
})

test_that('binary classification works',{
  d <- d %>%
    keepClasses(cls = 'day',classes = c('H','5'))
  
  rf <- randomForest(d,cls = 'day',binary = TRUE)
  
  expect_s4_class(rf,'RandomForest')
})

test_that('classification throws an error when less than 2 classes available',{
  expect_error(d %>% 
    keepClasses(cls = 'day',classes = 'H') %>% 
    randomForest(cls = 'day'))
})

test_that('random forest regression works',{
  rf <- randomForest(d,cls = 'injorder',perm = 3,returnModels = TRUE)
  rf_no_perm <- randomForest(d,cls = 'injorder')
  
  expect_s4_class(rf,'RandomForest')
  expect_identical(type(rf),'regression')
  expect_s3_class(metrics(rf),'tbl_df')
  expect_s3_class(importance(rf),'tbl_df')
  expect_s3_class(explanatoryFeatures(rf,metric = "IncNodePurity",value = 'p-value'),'tbl_df')
  expect_error(explanatoryFeatures(rf,metric = 'wrong'))
  expect_error(explanatoryFeatures(rf_no_perm,value = 'p-value'))
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
