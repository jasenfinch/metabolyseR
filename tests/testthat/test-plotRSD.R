library(metaboData)

context('plotRSD')

test_that('plotRSD works for Analysis class',{
  d <- new('Analysis')
  
  raw(d) <- analysisData(abr1$neg[,300:400],abr1$fact)
  
  p <- analysisParameters('pre-treatment')
  
  parameters(p,'pre-treatment') <- list(
    keep = list(classes = list(cls = 'day',classes = '5')),
    occupancyFilter = list(maximum = list(cls = 'day',occupancy = 2/3)),
    impute = list(all = list(occupancy = 2/3,
                             parallel = 'variables',
                             nCores = 2,
                             clusterType = getClusterType(),
                             seed = 1234))
  )
  
  pl <- plotRSD(d,cls = 'day',QCidx = '5',QCparameters = p)
  
  expect_identical(class(pl),c('patchwork','gg','ggplot'))
})

test_that('plotRSD works for Analysis class',{
  d <- analysisData(abr1$neg,abr1$fact)
  
  pl <- plotRSD(d,cls = 'day')
  
  expect_identical(class(pl),c('patchwork','gg','ggplot'))
})