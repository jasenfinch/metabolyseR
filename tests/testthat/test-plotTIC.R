library(metaboData)

context('plotTIC')

test_that('plotTIC works',{
  d <- new('Analysis')
  
  raw(d) <- analysisData(abr1$neg[,300:400],abr1$fact)
  
  pl <- plotTIC(d,by = 'injorder',colour = 'day')
  
  expect_identical(class(pl),c('gg','ggplot'))
})