library(metaboData)

d <- analysisData(abr1$neg[,200:250],abr1$fact)

context('univariate')

test_that('anova works',{
  d <- d %>%
    keepClasses(cls = 'day',classes = c('1','H'))
  res <- anova(d,cls = 'day')
  i <- importance(res)
  
  expect_true(class(res) == 'Univariate')
  expect_identical(class(i),c("tbl_df","tbl","data.frame"))
  expect_equal(ncol(i),10)
  expect_equal(nrow(i),51)
})

test_that('ttest works',{
  d <- d %>%
    keepClasses(cls = 'day',classes = c('1','H'))
  res <- ttest(d,cls = 'day')
  
  expect_true(class(res) == 'Univariate')
})

test_that('linearRegression works',{
  res <- linearRegression(d,cls = 'injorder')
  
  expect_true(class(res) == 'Univariate')
})
