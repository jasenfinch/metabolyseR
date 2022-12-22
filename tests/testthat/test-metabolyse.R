
test_that('metabolyse-works', {
  p <- analysisParameters()
  
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      occupancyFilter = 'maximum',
      transform = 'TICnorm')
  )
  
  changeParameter(p,'cls') <- 'day'
  
  cls1 <- metaboData::abr1$neg[metaboData::abr1$fact$class %in% c('1'),190:200][1:10,]
  cls2 <- metaboData::abr1$neg[metaboData::abr1$fact$class %in% c('6'),190:200][1:10,]
  dat <- rbind(cls1,cls2)
  
  inf1 <- metaboData::abr1$fact[abr1$fact$class %in% c('1'),][1:10,]
  inf2 <- metaboData::abr1$fact[abr1$fact$class %in% c('6'),][1:10,]
  info <- rbind(inf1,inf2)
  
  analysis <- metabolyse(dat,info,p,verbose = FALSE)

  expect_true(isS4(analysis))
  expect_true(class(analysis) == 'Analysis')
  expect_equal(nFeatures(analysis,type = 'raw'),11)
  expect_equal(nSamples(analysis,type = 'raw'),20)
  
  expect_s3_class(metrics(analysis),'tbl_df')
  expect_s3_class(predictions(analysis),'tbl_df')
  expect_s3_class(proximity(analysis),'tbl_df')
  expect_s3_class(mds(analysis),'tbl_df')
  expect_s3_class(roc(analysis),'tbl_df')
})

test_that('getPreTreatMethods errors if incorrect method specified',{
  expect_error(getPreTreatMethods('incorrect'))
})
