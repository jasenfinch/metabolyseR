test_that('plotExplanatoryHeatmap returns a plot for random forest classification Analysis',{
  
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
  
  d <- metabolyse(metaboData::abr1$neg[,200:300],
                  metaboData::abr1$fact,
                  p,
                  verbose = TRUE)
  
  pl_feat <- plotExplanatoryHeatmap(d,threshold = 0.5)
  pl_no_feat <- plotExplanatoryHeatmap(d,threshold = 0.5,featureNames = FALSE)
  pl_limit_features <- plotExplanatoryHeatmap(d,featureLimit = 10)
  pl_no_explan_feat <- plotExplanatoryHeatmap(d,threshold = -Inf)
  
  expect_type(pl_feat,'list')
  expect_type(pl_no_feat,'list')
  expect_type(pl_limit_features,'list')
  expect_null(pl_no_explan_feat[[1]])
})

test_that('plotExplanatoryHeatmap returns a plot for random forest regression',{
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250])
  x <- randomForest(d,cls = 'injorder',perm = 3)
  
  pl_feat <- plotExplanatoryHeatmap(x,metric = 'IncNodePurity')
  pl_no_feat <- plotExplanatoryHeatmap(x,
                                       metric = 'IncNodePurity',
                                       featureNames = FALSE)
  pl_limit_features <- plotExplanatoryHeatmap(x,
                                              metric = 'IncNodePurity',
                                              featureLimit = 10)
  
  expect_s3_class(pl_feat,"patchwork")
  expect_s3_class(pl_no_feat,"patchwork")
  expect_s3_class(pl_limit_features,'patchwork')
})

test_that('plotExplanatoryHeatmap works for linear regression',{
  pl <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250]) %>%
    linearRegression(cls = c('class','injorder')) %>%
    plotExplanatoryHeatmap()
  
  expect_s3_class(pl,'patchwork')
})

test_that('plotExplanatoryHeatmap method for lists throws an error when non modelling classes supplied',{
  expect_error(list(wrong = list(wrong = 1)) %>%
                 plotExplanatoryHeatmap())
})

test_that('plotExplanatoryHeatmap throws error when unsupervised random forest supplied',{
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250]) %>%
    randomForest(cls = NULL)
  expect_error(plotExplanatoryHeatmap(d))
})
