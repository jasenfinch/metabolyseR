
test_that('AnalysisParameters show method works',{
  expect_output(new('AnalysisParameters') %>%
                  print(),'Parameters:')
})

test_that('AnalysisData show method works',{
  expect_output(new('AnalysisData') %>%
                  print(),'AnalysisData object containing:')
})

test_that('Analysis show method works',{
  expect_output(new('Analysis') %>%
                  print(),'Analysis:')
})

test_that('RandomForest class show method works',{
  object <- new('RandomForest',
                type = 'classification',
                response = 'class')
  expect_output(print(object),
                'Random forest classification')
})

test_that('Univariate class show method works',{
  expect_output(new('Univariate') %>%
                  print(),'Univariate  analysis')
})
