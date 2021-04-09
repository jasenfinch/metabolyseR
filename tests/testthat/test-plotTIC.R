library(metaboData)

context('plotTIC')

test_that('plotTIC works',{
  d <- new('Analysis')
  
  raw(d) <- analysisData(abr1$neg[,300:400],abr1$fact)
  
  pl_scatter <- plotTIC(d,by = 'injorder',colour = 'day')
  pl_boxplot <- plotTIC(d,by = 'day',colour = 'day')
  
  expect_s3_class(pl_scatter,'ggplot')
  expect_s3_class(pl_boxplot,'ggplot')
})
