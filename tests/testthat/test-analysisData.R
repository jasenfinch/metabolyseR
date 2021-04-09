
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

test_that(str_c('wrong type throws error for sample information',
                ' retrieval from Analysis'),{
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

test_that(str_c('sample information assignment for Analysis throws error',
                ' with wrong type'),{
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

test_that(str_c('analysisData throws error with sample information and',
                ' data row mismatch'),{
  expect_error(analysisData(abr1$neg[1:2,],abr1$fact))
})

test_that('features throws an error with incorrect type',{
  expect_error(new('Analysis') %>%
                 features(type = 'wrong'))
})

test_that('nFeatures throws an error with incorrect type',{
  expect_error(new('Analysis') %>%
                 nFeatures(type = 'wrong'))
})

test_that('nSamples throws an error with incorrect type',{
  expect_error(new('Analysis') %>%
                 nSamples(type = 'wrong'))
})

test_that('features, nFeatures and nSamples can be extracted from Analysis',{
  p <- analysisParameters(elements = 'pre-treatment')
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'classes'
    )
  )
  changeParameter(p,'classes') <- c(1,6)
  
  d <- metabolyse(abr1$neg[,190:200],abr1$fact,p,verbose = FALSE)
  
  feat_raw <- features(d,type = 'raw')
  feat_pre_treated <- features(d,type = 'pre-treated')
  nFeat_raw <- nFeatures(d,type = 'raw')
  nFeat_pre_treated <- nFeatures(d,type = 'pre-treated')
  nSamples_raw <- nSamples(d,type = 'raw')
  nSamples_pre_treated <- nSamples(d,type = 'pre-treated')
  
  expect_length(feat_raw,11)
  expect_length(feat_pre_treated,11)
  expect_equal(nFeat_raw,11)
  expect_equal(nFeat_pre_treated,11)
  expect_equal(nSamples_raw,120)
  expect_equal(nSamples_pre_treated,40)
})

test_that('number of features and samples correctly returned from Analysis',{
  columns <- 190:200
  p <- analysisParameters(elements = 'pre-treatment')
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'classes'
    )
  )
  changeParameter(p,'classes') <- c(1,6)
  
  d <- metabolyse(abr1$neg[,columns],abr1$fact,p,verbose = FALSE)
  
  expect_equal(nFeatures(d),length(columns))
  expect_equal(nSamples(d),nrow(abr1$neg))
})
