context('correlations')

test_that('correlations-works', {
  
  p <- analysisParameters(elements = c('correlations'))
  changeParameter(p,'method') <- 'spearman'
  
  d <- metabolyse(abr1$neg[,200:300],abr1$fact,p,verbose = FALSE) %>%
    analysisResults('correlations')
  
  expect_s3_class(d,'tbl_df')
})


