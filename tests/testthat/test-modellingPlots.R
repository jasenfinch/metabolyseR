
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

suppressWarnings(
  classification_rf_res <- d %>%
    randomForest(cls = c('day','name'))
)

regression_rf_res <- d %>%
  randomForest(cls = 'injorder')

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

test_that('plotImportance for Univariate class throws an error when the incorrect response is specified',{
  expect_error(plotImportance(ttest_res,'class'))
})

test_that('plotImportance for Univariate class throws an error when the incorrect response is specified when muliple responses available',{
  expect_error(plotImportance(ttest(d,cls = c('day','class')),'wrong'))
})


test_that('plotImportance for RandomForest class throws an error when incorrect metric specified',{
  expect_error(plotImportance(classification_rf_res,metric = 'wrong'))
})

test_that('an error is thrown when an non incorrect class present in list for plotImportance',{
  d <- c(classification_rf_res,list('wrong'))
  expect_error(plotImportance(d))
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

test_that('an error is thrown from plotMetrics for unsupervised random forest',{
  expect_error(plotMetrics(unsupervised_rf_res))
})

test_that('An error is thrown from plotMetrics when non RandomForest object included in list',{
  d <- c(classification_rf_res,list('wrong'))
  expect_error(plotMetrics(d))
})

test_that('plotMDS works on a list of random forest objects',{
  pl <- plotMDS(classification_rf_res)
  
  expect_s3_class(pl,'patchwork')
})

test_that('plotMDS throws an error when an incorrect cls specified',{
  expect_error(plotMDS(unsupervised_rf_res,cls ='wrong'))
})

test_that('plotMDS throws an error when non RandomForest object included in list',{
  d <- c(classification_rf_res,list('wrong'))
  expect_error(plotMDS(d))
})

test_that('plotROC throws an error when non classification random forest specified',{
  expect_error(plotROC(unsupervised_rf_res))
})
