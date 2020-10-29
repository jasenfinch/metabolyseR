
context('analysis data and sample information methods')

test_that('wrong type throws error for data retrieval from Analysis',{
  d <- new('Analysis')
  expect_error(dat(d,type = 'wrong'))
})

test_that('correctly retrieve raw data from Analysis',{
  d <- new('Analysis') %>%
    dat(type = 'raw')
  expect_s3_class(d,'tbl_df')
})

test_that('correctly retrieve pre-treated data from Analysis',{
  d <- new('Analysis') %>%
    dat(type = 'pre-treated')
  expect_s3_class(d,'tbl_df')
})

test_that('data assignment for Analysis throws error with wrong type',{
  d <- new('Analysis')
  expect_error(dat(d,type = 'wrong') <- tibble())
})

test_that('assign raw data for Analysis',{
  d <- new('Analysis')
  dat(d,type = 'raw') <- tibble(a = 1)
  rows <- d %>%
    dat(type = 'raw') %>%
    nrow()
  expect_equal(rows,1)
})

test_that('assign pre-treated data for Analysis',{
  d <- new('Analysis')
  dat(d,type = 'pre-treated') <- tibble(a = 1)
  rows <- d %>%
    dat(type = 'pre-treated') %>%
    nrow()
  expect_equal(rows,1)
})

test_that('wrong type throws error for sample information retrieval from Analysis',{
  d <- new('Analysis')
  expect_error(sinfo(d,type = 'wrong'))
})

test_that('correctly retrieve raw sample information from Analysis',{
  d <- new('Analysis') %>%
    sinfo(type = 'raw')
  expect_s3_class(d,'tbl_df')
})

test_that('correctly retrieve pre-treated sample information from Analysis',{
  d <- new('Analysis') %>%
    sinfo(type = 'pre-treated')
  expect_s3_class(d,'tbl_df')
})

test_that('sample information assignment for Analysis throws error with wrong type',{
  d <- new('Analysis')
  expect_error(sinfo(d,type = 'wrong') <- tibble())
})

test_that('assign raw sample information for Analysis',{
  d <- new('Analysis')
  sinfo(d,type = 'raw') <- tibble(a = 1)
  rows <- d %>%
    sinfo(type = 'raw') %>%
    nrow()
  expect_equal(rows,1)
})

test_that('assign pre-treated sample information for Analysis',{
  d <- new('Analysis')
  sinfo(d,type = 'pre-treated') <- tibble(a = 1)
  rows <- d %>%
    sinfo(type = 'pre-treated') %>%
    nrow()
  expect_equal(rows,1)
})

test_that('analysisResults throws error on wrong type',{
  d <- new('Analysis')
  expect_error(analysisResults(d,element = 'wrong'))
})

test_that('analysis results returned for pre-treated data from Analysis',{
  d <- new('Analysis') %>%
    analysisResults(element = 'pre-treatment')
  expect_s4_class(d,'AnalysisData')
})
