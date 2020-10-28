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
  changeParameter(p,'nCores') <- 2
  
  analysis <- metabolyse(abr1$neg[,1:200],
                         abr1$fact,
                         p,verbose = FALSE)
  
  analysis <- reAnalyse(analysis,
                        parameters = analysisParameters('correlations'),
                        verbose = FALSE)
  
  expect_identical(class(analysis@correlations),c("tbl_df","tbl","data.frame"))
})
