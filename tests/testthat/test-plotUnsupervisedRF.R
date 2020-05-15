
context('plotUnsupervisedRF')

test_that('plotUnsupervisedRF works',{
  
  p <- analysisParameters(c('pre-treatment'))
  
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'classes',
      occupancyFilter = 'maximum',
      transform = 'TICnorm' 
    )
  )
  
  changeParameter(p,'classes') <- c('H','1')
  changeParameter(p,'cls') <- 'day'
  
  d <- metabolyse(abr1$neg[,1:200],abr1$fact,p)
  
  pl <- plotUnsupervisedRF(d,cls = 'day')
  
  expect_true(identical(class(pl),c("gg","ggplot")))
})