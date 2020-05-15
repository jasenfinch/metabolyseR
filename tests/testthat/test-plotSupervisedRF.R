
context('plotSupervisedRF')

test_that('plotSupervisedRF works',{
  
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
  
  pl <- plotSupervisedRF(d,cls = 'day')
  
  expect_true(identical(class(pl),c('patchwork',"gg","ggplot")))
})