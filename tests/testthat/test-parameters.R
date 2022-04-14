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
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(transform = 'TICnorm'))
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
                   c("method","pAdjustMethod","corPvalue",'minCoef','maxCor'))
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

test_that('an error is thrown when a non list object is supplied to parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'pre-treatment') <- 'wrong')
})

test_that('an error is thrown when an incorrect pre-treatment elements are supplied to parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'pre-treatment') <- list(wrong = list()))
})

test_that('an error is thrown when an incorrect methods are supplied to pre-treatment parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'pre-treatment') <- list(remove = list(wrong = 'wrong')))
})

test_that('an error is thrown when an incorrect modelling method is supplied to parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'modelling') <- list(wrong = list()))
})

test_that('an error is thrown when an incorrect modelling parameter is supplied to modelling parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'modelling') <- list(randomForest = list(wrong = 'wrong')))
})

test_that('an error is thrown when incorrect arguments are supplied to correlation parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'correlations') <- list(wrong = 'wrong'))
})

test_that('an error is thrown when incorrect correlation method is supplied to parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'correlations') <- list(method = 'wrong'))
})

test_that('an error is thrown when incorrect p value adjustment method is supplied to correlation parameters',{
  p <- analysisParameters()
  expect_error(parameters(p,'correlations') <- list(pAdjustMethod = 'wrong'))
})

test_that('an error is thrown when a non-numeric value is supplied to correlation p value threshold parameter',{
  p <- analysisParameters()
  expect_error(parameters(p,'correlations') <- list(corPvalue = 'wrong'))
})

test_that('an error is thrown when an incorrect analysis element is supplied changeParameter',{
  p <- analysisParameters()
  expect_error(changeParameter(p,elements = 'wrong') <- 'wrong')
})

test_that('an error is thrown when a non list object is supplied to preTreatmentParameters',{
  p <- analysisParameters()
  expect_error(preTreatmentParameters('wrong'))
})


test_that('an error is thrown when incorrect elements are supplied to preTreatmentParameters',{
  p <- analysisParameters()
  expect_error(preTreatmentParameters(list(wrong = 'wrong')))
})

test_that('an error is thrown when incorrect methods are supplied to preTreatmentParameters',{
  p <- analysisParameters()
  expect_error(preTreatmentParameters(list(remove = 'wrong')))
})

test_that('an error is thrown when a non character vector is supplied to modelling Parameters',{
  p <- analysisParameters()
  expect_error(modellingParameters(1))
})

test_that('an error is thrown when an incorrect method is supplied to modelling Parameters',{
  p <- analysisParameters()
  expect_error(modellingParameters('wrong'))
})