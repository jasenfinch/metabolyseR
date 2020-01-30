library(metaboData)

context('plotOccupancy')

test_that('plotOccupancy works for Analysis class',{
  d <- new('Analysis')
  
  raw(d) <- analysisData(abr1$neg[,300:400],abr1$fact)
  
  pl <- plotOccupancy(d,cls = 'day')
  
  expect_identical(class(pl),c('patchwork','gg','ggplot'))
})

test_that('plotccupancy works for Analysis class',{
  d <- analysisData(abr1$neg,abr1$fact)
  
  pl <- plotOccupancy(d,cls = 'day')
  
  expect_identical(class(pl),c('patchwork','gg','ggplot'))
})