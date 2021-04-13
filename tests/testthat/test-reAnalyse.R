library(metaboData)

context('reAnalyse')

test_that('reAnalyse works',{
  p <- analysisParameters(c('pre-treatment','modelling'))
  
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(occupancyFilter = 'maximum',
         transform = 'TICnorm')
  )
  parameters(p,'modelling') <- modellingParameters('anova')
  
  changeParameter(p,'cls') <- 'day'
  
  analysis <- metabolyse(abr1$neg[,1:200],
                         abr1$fact,
                         p,verbose = FALSE)
  
  analysis <- reAnalyse(analysis,
                        parameters = analysisParameters('correlations'),
                        verbose = TRUE)
  
  expect_s3_class(analysisResults(analysis,'correlations'),"tbl_df")
  expect_s3_class(importance(analysis),"tbl_df")
  expect_s3_class(explanatoryFeatures(analysis),"tbl_df")
})
