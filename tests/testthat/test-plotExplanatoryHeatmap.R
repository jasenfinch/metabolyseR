library(metaboData)

context('plotExplanatoryHeatmap')

test_that(str_c('plotExplanatoryHeatmap returns a plot for random',
                ' forest classification Analysis'),{
  
  p <- analysisParameters(elements = c('pre-treatment','modelling'))
  parameters(p,'pre-treatment') <- preTreatmentParameters(
    list(
      keep = 'classes',
      occupancyFilter = 'maximum'
    )
  )
  changeParameter(p,'classes') <- c('H','5')
  
  parameters(p,'modelling') <- c(modellingParameters('randomForest'),
                                 modellingParameters('ttest')) 

  changeParameter(p,'cls') <- 'day'
  
  d <- metabolyse(abr1$neg[,200:300],abr1$fact,p,verbose = TRUE)
  
  pl_feat <- plotExplanatoryHeatmap(d,threshold = 0.5)
  pl_no_feat <- plotExplanatoryHeatmap(d,threshold = 0.5,featureNames = FALSE)
  
  expect_true(is.list(pl_feat))
  expect_true(is.list(pl_no_feat))
})

test_that('plotExplanatoryHeatmap returns a plot for random forest regression',{
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250])
  x <- randomForest(d,cls = 'injorder',perm = 3,nCores = 2)
  
  pl_feat <- plotExplanatoryHeatmap(x,metric = 'IncNodePurity')
  pl_no_feat <- plotExplanatoryHeatmap(x,
                                       metric = 'IncNodePurity',
                                       featureNames = FALSE)
  
  expect_s3_class(pl_feat,"patchwork")
  expect_s3_class(pl_no_feat,"patchwork")
})

test_that('plotExplanatoryHeatmap returns a plot for > 500 features',{
  pl_classi <- analysisData(abr1$neg,abr1$fact) %>%
    keepClasses(cls = 'day',classes = c('H','5')) %>%
    keepFeatures(features = features(.)[400:1000]) %>%
    {
      . <- clsReplace(.,cls = 'class',clsExtract(.,cls = 'day'))
      return(.)
    } %>%
    randomForest(cls = c('day','class')) %>%
    plotExplanatoryHeatmap(threshold = 2)
  
  pl_reg <- analysisData(abr1$neg,abr1$fact) %>%
    keepClasses(cls = 'day',classes = c('H','5')) %>%
    keepFeatures(features = features(.)[400:1000]) %>%
    occupancyMaximum(cls = 'day') %>%
    randomForest(cls = 'injorder') %>%
    plotExplanatoryHeatmap(threshold = -1,metric = 'IncNodePurity')
  
  expect_true(is.list(pl_classi))
  expect_s3_class(pl_reg,"patchwork")
})

test_that('plotExplanatoryHeatmap works for linear regression',{
  pl <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250]) %>%
    linearRegression(cls = c('class','injorder')) %>%
    plotExplanatoryHeatmap()
  expect_s3_class(pl,'patchwork')
})

test_that('plotExplanatoryHeatmap method for lists throws an error when non modelling classes supplied',{
  expect_error(list(wrong = list()) %>%
                 plotExplanatoryHeatmap())
})

test_that('plotExplanatoryHeatmap throws error when unsupervised random forest supplied',{
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250]) %>%
    randomForest(cls = NULL)
  expect_error(plotExplanatoryHeatmap(d))
})
