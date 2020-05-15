
context('modellingPlots')

d <- analysisData(abr1$neg[,1:200],abr1$fact) %>%
  keepClasses(cls = 'day',classes = c('H','1','2')) %>%
  occupancyMaximum(cls = 'day') %>%
  transformTICnorm()

anova_res <- d %>%
  anova(cls = 'day')

unsupervised_rf_res <- d %>%
  randomForest(cls = NULL)

classification_rf_res <- d %>%
  randomForest(cls = 'day')

regression_rf_res <- d %>%
  randomForest(cls = 'class')

test_that('plotImportance works for Univariate class',{
  pl <- plotImportance(anova_res,response = 'day')
  
  expect_identical(class(pl),c("gg","ggplot"))
})

test_that('plotImportance works for unsupervised random forest',{
  pl <- plotImportance(unsupervised_rf_res)
  
  expect_identical(class(pl),'list')
  expect_identical(class(pl[[1]]),c('gg','ggplot'))
})

test_that('plotImportance works for random forest classification',{
  pl <- plotImportance(classification_rf_res)
  
  expect_identical(class(pl),'list')
  expect_identical(class(pl$day),c('gg','ggplot'))
})

test_that('plotImportance works for random forest regression',{
  pl <- plotImportance(regression_rf_res)
  
  expect_identical(class(pl),'list')
  expect_identical(class(pl$class),c('gg','ggplot'))
})

test_that('plotMeasures works for random forest classification',{
  pl <- classification_rf_res %>%
    plotMeasures()
  
  expect_identical(class(pl),'list')
  expect_identical(class(pl$day),c('gg','ggplot'))
})

test_that('plotMeasures works for random forest regression',{
  pl <- regression_rf_res %>%
    plotMeasures()
  
  expect_identical(class(pl),'list')
  expect_identical(class(pl$class),c('gg','ggplot'))
})

# plMDS <- plotMDS(rf$day,cls = 'day')
# plROC <- plotROC(rf$day)
# plMeasures <- plotMeasures(rf$day,response = 'day')
# plImportance <- plotImportance(rf$day)
# 
# expect_identical(class(plMDS),c("gg","ggplot"))
# expect_identical(class(plROC),c("gg","ggplot"))
# expect_identical(class(plMeasures),c("gg","ggplot"))
# expect_identical(class(plImportance),c("gg","ggplot"))