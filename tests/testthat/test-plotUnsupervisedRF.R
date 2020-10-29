
context('plotUnsupervisedRF')

test_that('plotUnsupervisedRF works for both raw and pre-treated Analysis data',{
  
  p <- analysisParameters(elements = 'pre-treatment')
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'classes'
    )
  )
  changeParameter(p,'classes') <- c(1,6)
  
  d <- metabolyse(abr1$neg[,190:200],abr1$fact,p,verbose = FALSE)
  
  pl_raw <- plotUnsupervisedRF(d,label = 'injorder',type = 'raw')
  pl_pre_treated <- plotUnsupervisedRF(d,label = 'injorder',type = 'pre-treated')
  
  expect_s3_class(pl_raw,'ggplot')
  expect_s3_class(pl_pre_treated,'ggplot')
})

test_that('plotUnsupervisedRF throws error when wrong type specified for Analysis',{
  d <- new('Analysis')
  expect_error(plotUnsupervisedRF(d,type = 'wrong'))
})

