
context('plotSupervisedRF')

test_that('plotSupervisedRF works for both raw and pre-treated Analysis data',{
  
  p <- analysisParameters(elements = 'pre-treatment')
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'classes'
    )
  )
  changeParameter(p,'classes') <- c(1,6)
  
  d <- metabolyse(abr1$neg[,190:200],abr1$fact,p,verbose = FALSE)
  
  pl_raw <- plotSupervisedRF(d,cls = 'day',label = 'injorder',type = 'raw')
  pl_pre_treated <- plotSupervisedRF(d,cls = 'day',label = 'injorder',type = 'pre-treated')
  
  expect_s3_class(pl_raw,'ggplot')
  expect_s3_class(pl_pre_treated,'ggplot')
})

test_that('plotSupervisedRF throws error when wrong type specified for Analysis',{
  d <- new('Analysis')
  expect_error(plotSupervisedRF(d,type = 'wrong'))
})

test_that('plotSupervisedRF plots without ROC',{
  pl <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[300:400]) %>%
    plotSupervisedRF(cls = 'day',ROC = FALSE)
  expect_s3_class(pl,'ggplot')
})

test_that('plotting skipped when errors encountered during random forest training',{
  expect_warning(pl <- analysisData(abr1$neg,abr1$fact) %>%
    keepClasses(cls = 'day',classes = 'H') %>% 
    plotSupervisedRF(cls = 'day'))
})
