library(metaboData)

context('plotRSD')

test_that('plotRSD returns a plot',{
  d <- analysisData(abr1$neg[,300:400],abr1$fact)
  
  p <- analysisParameters('preTreat')
  p@preTreat <- list(
    keep = list(classes = list(cls = 'day',classes = '5')),
    occupancyFilter = list(maximum = list(cls = 'day',occupancy = 2/3)),
    impute = list(all = list(occupancy = 2/3,parallel = 'variables',nCores = 2,clusterType = getClusterType(),seed = 1234))
  )
  
  pl <- plotRSD(d,cls = 'day',QCidx = '5',QCparameters = p)
  
  expect_identical(class(pl),c('patchwork','gg','ggplot'))
})