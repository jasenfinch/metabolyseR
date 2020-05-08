library(metaboData)

context('parameters')

p <- analysisParameters()
a <- new('Analysis')

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
