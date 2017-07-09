library(FIEmspro)

context('correlations')

test_that('correlations-works', {
  data(abr1)
  correlations <- metabolyse(abr1$neg[,150:200],abr1$fact,analysisParameters('correlations'))
  expect_true(class(correlations@correlations)[1] == 'tbl_df')
})

