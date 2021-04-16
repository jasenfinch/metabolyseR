context('plotLDA')

test_that('plotLDA works for both raw and pre-treated Analysis data',{
  
  p <- analysisParameters(elements = 'pre-treatment')
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      remove = 'classes',
      occupancyFilter = 'maximum'
    )
  )
  changeParameter(p,'classes') <- c(1,6)
  changeParameter(p,'cls') <- 'day'
  
  d <- metabolyse(abr1$neg[,200:300],abr1$fact,p,verbose = FALSE)
  
  pl_raw <- plotLDA(d,cls = 'day',label = 'injorder',type = 'raw')
  pl_pre_treated <- plotLDA(d,cls = 'day',label = 'injorder',type = 'pre-treated')
  
  expect_s3_class(pl_raw,'ggplot')
  expect_s3_class(pl_pre_treated,'ggplot')
})

test_that('plotLDA throws error when wrong type specified for Analysis',{
  d <- new('Analysis')
  expect_error(plotLDA(d,type = 'wrong'))
})

test_that('plotLDA throws error when number of classes is less than 2',{
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepClasses(classes = 1)
  expect_error(plotLDA(d))
})

test_that('plotLDA plots for 2 classes',{
  pl <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[300:400]) %>%
    keepClasses(classes = c(1,6)) %>%
    plotLDA(cls = 'day')
  expect_s3_class(pl,'ggplot')
})
