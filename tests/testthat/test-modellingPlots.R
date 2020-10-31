
context('modellingPlots')

d <- analysisData(abr1$neg,abr1$fact) %>%
  keepFeatures(features = features(.)[seq_len(200)]) %>%
  keepClasses(cls = 'day',classes = c('H','1','2')) %>%
  occupancyMaximum(cls = 'day') %>%
  transformTICnorm()

ttest_res <- d %>%
  ttest(cls = 'day')

unsupervised_rf_res <- d %>%
  randomForest(cls = NULL)

classification_rf_res <- d %>%
  randomForest(cls = c('day','name'))

regression_rf_res <- d %>%
  randomForest(cls = 'class')

test_that('plotImportance works for Univariate class',{
  pl <- plotImportance(ttest_res,response = 'day')
  
  expect_s3_class(pl,'patchwork')
})

test_that('plotImportance throws an error when incorrect response specified',{
  expect_error(plotImportance(anova_res,response = 'wrong'))
})

test_that('plotImportance works for unsupervised random forest',{
  pl <- plotImportance(unsupervised_rf_res)
  
  expect_identical(class(pl),c('gg','ggplot'))
})

test_that('plotImportance works for random forest classification',{
  pl <- plotImportance(classification_rf_res)
  
  expect_identical(class(pl),'list')
})

test_that('plotImportance works for random forest regression',{
  pl <- plotImportance(regression_rf_res,metric = '%IncMSE')
  
  expect_identical(class(pl),c('gg','ggplot'))
})

test_that('plotMetrics works for random forest classification',{
  pl <- classification_rf_res %>%
    plotMetrics()
  
  expect_identical(class(pl),'list')
})

test_that('plotMetrics works for random forest regression',{
  pl <- regression_rf_res %>%
    plotMetrics()
  
  expect_identical(class(pl),c('gg','ggplot'))
})

test_that('plotMDS works on a list of random forest objects',{
  pl <- plotMDS(classification_rf_res)
  
  expect_s3_class(pl,'patchwork')
})
