library(metaboData)

context('plotExplanatoryHeatmap')

test_that('plotExplanatoryHeatmap returns a plot',{
  
  pl <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[190:220]) %>%
    keepClasses(classes = c('1','6')) %>%
    randomForest(cls = 'day',nCores = 1) %>%
    plotExplanatoryHeatmap(threshold = 0.5)
  
  expect_identical(class(pl),c("patchwork","plot_filler","gg","ggplot"))
})

test_that('plotExplanatoryHeatmap returns a plot',{
  d <- analysisData(abr1$neg,abr1$fact) %>%
    keepFeatures(features = features(.)[200:250])
  x <- randomForest(d,cls = 'injorder',perm = 3,nCores = 2)
  
  pl <- plotExplanatoryHeatmap(x,metric = 'IncNodePurity')
  
  expect_identical(class(pl),c("patchwork","plot_filler","gg","ggplot"))
})