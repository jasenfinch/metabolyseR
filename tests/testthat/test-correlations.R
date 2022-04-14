context('correlations')

test_that('correlations-works', {
  
  p <- analysisParameters(elements = c('correlations'))
  changeParameter(p,'method') <- 'spearman'
  
  d <- metabolyse(abr1$neg[,200:300],abr1$fact,p) %>%
    analysisResults('correlations')
  
  expect_s3_class(d,'tbl_df')
})

test_that('correlations errors when incorrect method specified',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  
  expect_error(correlations(d,method = 'wrong'))
})

test_that('correlations errors when incorrect p value adjustment method specified',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  
  expect_error(correlations(d,pAdjustMethod = 'wrong'))
})

test_that('correlations errors when incorrect correlation p value threshold specified',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  
  expect_error(correlations(d,corPvalue = 'wrong'))
})

test_that('the number of returned correlations can be thresholded',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact)
  
  n_correlations <- correlations(d,maxCor = 2) %>% 
    nrow()
  
  expect_equal(n_correlations,2)
})