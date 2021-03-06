library(metaboData)

context('metabolyse')

test_that('metabolyse-works', {
  p <- analysisParameters()
  
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      occupancyFilter = 'maximum',
      transform = 'TICnorm')
  )
  
  changeParameter(p,'cls') <- 'day'
  
  cls1 <- abr1$neg[abr1$fact$class %in% c('1'),190:200][1:10,]
  cls2 <- abr1$neg[abr1$fact$class %in% c('6'),190:200][1:10,]
  dat <- rbind(cls1,cls2)
  inf1 <- abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  analysis <- metabolyse(dat,info,p,verbose = FALSE)

  expect_true(isS4(analysis))
  expect_true(class(analysis) == 'Analysis')
  expect_equal(nFeatures(analysis),11)
  expect_equal(nSamples(analysis),20)
  
  expect_s3_class(metrics(analysis),'tbl_df')
})
