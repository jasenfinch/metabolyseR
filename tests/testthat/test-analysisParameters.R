
context('analysisParameters')

test_that('analysisParameters works', {
  p <- analysisParameters()
  expect_true(isS4(p))
  expect_true(class(p) == 'AnalysisParameters')
  expect_equal(slotNames(p),analysisElements())
})