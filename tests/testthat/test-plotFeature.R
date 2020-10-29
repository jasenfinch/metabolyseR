library(metaboData)

context('plotFeature')

test_that('plotFeature plot data from Analysis',{
  p <- analysisParameters(elements = 'pre-treatment')
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'features',
      keep = 'classes'
    )
  )
  changeParameter(p,'features') <- 'N133'
  changeParameter(p,'classes') <- c(1,6)
  
  d <- metabolyse(abr1$neg,abr1$fact,p,verbose = FALSE)
  
  pl_raw <- d %>%
    plotFeature(feature = 'N133',cls = 'day',type = 'raw')
  
  pl_pre_treated <- d %>%
    plotFeature(feature = 'N133',cls = 'day',type = 'pre-treated',label = 'injorder')
  
  expect_s3_class(pl_raw,'ggplot')
  expect_s3_class(pl_pre_treated,'ggplot')
})

test_that('plotFeature throws error when wrong type specified for Analysis',{
  d <- new('Analysis')
  expect_error(plotFeature(d,type = 'wrong'))
})

test_that('plotFeature throws error when wrong feature specified',{
  d <- analysisData(abr1$neg,abr1$fact)
  expect_error(plotFeature(d,feature = 'wrong'))
})

test_that('plotFeature plots continuous cls',{
  d <- analysisData(abr1$neg,abr1$fact)
  pl <- plotFeature(d,feature = 'N133',cls = 'injorder')
  expect_s3_class(pl,'ggplot')
})

test_that('plotFeature works for a multiple of 12 classes',{
  d <- analysisData(abr1$neg,abr1$fact)
  classes <- clsExtract(d,cls = 'name') %>%
    unique() %>%
    .[1:24]
  
  pl <- d %>%
    keepClasses(cls = 'name',classes = classes) %>%
    plotFeature(feature = 'N133',cls = 'name')
  expect_s3_class(pl,'ggplot')
})

test_that('plotFeature works for more than 12 classes',{
  d <- analysisData(abr1$neg,abr1$fact)
  pl <- plotFeature(d,feature = 'N133',cls = 'name')
  expect_s3_class(pl,'ggplot')
})
