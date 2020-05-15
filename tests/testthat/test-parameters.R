library(metaboData)

context('parameters')

p <- analysisParameters()
a <- new('Analysis')

test_that('analysisParameters works',{
  p <- analysisParameters()
  
  expect_s4_class(p,'AnalysisParameters')
  expect_error(analysisParameters(1))
  expect_error(analysisParameters('an_element'))
})

test_that('parameters assigned for Analysis objects',{
  parameters(a) <- p
  pa <- a %>%
    parameters() %>%
    parameters('pre-treatment')
  expect_equal(length(pa),length(parameters(p,'pre-treatment')))
})

test_that('parameters extracted for Analysis objects',{
  parameters(a) <- p
  expect_identical(parameters(a),p)
})

test_that('parameters extracted for AnalysisParameters objects',{
  params <- parameters(p,'pre-treatment')
  expect_length(params,4)
  expect_error(parameters(p,'an_element'))
  expect_error(parameters(p,'an_element') <- analysisParameters())
})

test_that('pre-treatment parameters assigned for AnalysisParameters',{
  parameters(p,'pre-treatment') <- preTreatmentParameters(list(transform = 'TICnorm'))
  expect_identical(parameters(p,'pre-treatment') %>% names(),'transform')
})

test_that('modelling parameters assigned for AnalysisParameters',{
  parameters(p,'modelling') <- modellingParameters('anova')
  expect_identical(parameters(p,'modelling') %>% names(),'anova')
})

test_that('correlations parameters assigned for AnalysisParameters',{
  parameters(p,'correlations') <- correlationsParameters()
  expect_identical(parameters(p,'correlations') %>% 
                     names(),
                   c("method","pAdjustMethod","corPvalue"))
})

test_that('parameters correctly exported and parsed',{
  a <- new('Analysis')
  parameters(a) <- analysisParameters()
  
  dir <- tempdir()
  file <- str_c(dir,'/','analysis_parameters.yaml')
  
  exportParameters(a,file)
  
  pp <- parseParameters(file)
  
  expect_true(file.exists(file))
  expect_s4_class(pp,'AnalysisParameters')
})
