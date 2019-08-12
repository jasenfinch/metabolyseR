library(metaboData)

context('univariate')

test_that('ttest works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact) %>%
    keepClasses(cls = 'day',classes = c('1','H'))
  res <- ttest(d,cls = 'day')
  
  expect_true(class(res) == 'Univariate')
})

test_that('linearRegression works',{
  d <- analysisData(abr1$neg[,200:250],abr1$fact) 
  res <- linearRegression(d,cls = 'injorder')
  
  expect_true(class(res) == 'Univariate')
})